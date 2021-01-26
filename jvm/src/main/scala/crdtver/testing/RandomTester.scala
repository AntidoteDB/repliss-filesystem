package crdtver.testing

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import crdtver.{Repliss, RunArgs}
import crdtver.Repliss.QuickcheckCounterexample
import crdtver.language.AtomicTransform
import crdtver.language.TypedAst.{AnyType, BoolType, CallIdType, FunctionType, IdType, InProgram, InTypeExpr, IntType, InvocationIdType, InvocationInfoType, InvocationResultType, OperationType, SimpleType, SomeOperationType}
import crdtver.testing.Interpreter._
import crdtver.language.TypedAst._
import crdtver.utils.{ConcurrencyUtils, Helper}

import scala.collection.immutable.{::, Nil}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Random, Success}
import scala.concurrent.duration._

/**
 * This class is responsible for executing random tests on a Repliss program.
 */
class RandomTester(prog: InProgram, runArgs: RunArgs) {

  // custom data types can have values 0 <= x < domainSize
  val domainSize: Int = runArgs.quickcheckDomainSize

  // maximum number of known ids for generating random values
  val maxUsedIds: Int = runArgs.quickcheckMaxUsedIds

  val interpreter = new Interpreter(prog, runArgs, domainSize)


  val debug = false

  def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }


  sealed abstract class ActionProvider {
    def discardLast(): Unit

    def nextAction(state: State): Option[Action]
  }

  class TraceActionProvider(trace: List[Action]) extends ActionProvider {
    private var exeutedActions = List[Action]()
    private var maxGivenId = 0
    private var todo = trace

    def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast(): Unit = {
      debugLog(s"### discarding ${exeutedActions.head}")
      exeutedActions = exeutedActions.tail
    }

    override def nextAction(state: State): Option[Action] = {
      todo match {
        case Nil => None
        case action1 :: todo2 =>
          val action = action1 match {
            case InvariantCheck(invocationId) =>
              if (state.localStates.contains(invocationId)) {
                debugLog(s"### keeping check at $invocationId")
                action1
              } else {
                val alternative = state.localStates.keys.headOption.getOrElse(invocationId)
                debugLog(s"### changing from $invocationId to $alternative")
                InvariantCheck(alternative)
              }
            case _ => action1
          }

          exeutedActions = action :: exeutedActions
          todo = todo2
          Some(action)
      }
    }
  }

  class RandomActionProvider(limit: Int, seed: Int = 0) extends ActionProvider {
    // trace of executed actions,  newest actions first
    private var exeutedActions = List[Action]()
    private val rand = new Random(seed)
    private var maxGivenId = 0

    def getTrace(): List[Action] = exeutedActions.reverse

    override def discardLast(): Unit = {
      exeutedActions = exeutedActions.tail
    }

    override def nextAction(state: State): Option[Action] = {
      if (Thread.currentThread().isInterrupted) {
        debugLog(s"$seed cancelled execution")
        return None
      }
      if (exeutedActions.size > limit) {
        debugLog(s"$seed found no counter example")
        return None
      }
      val action = randomAction(state)
      exeutedActions = action :: exeutedActions
      debugLog(s"$seed generating action ${exeutedActions.size}/$limit")
      Some(action)
    }


    def newRandomInvariantCheck(state: State): Action = {
      val id = pickRandom(state.localStates.keys.toList)(rand)
      InvariantCheck(id)
    }

    def randomAction(state: State): Action = {
      if (state.localStates.nonEmpty && exeutedActions.size == limit) {
        newRandomInvariantCheck(state)
      }


      val waitFors = getLocalWaitingFors(state)
      val i = rand.nextInt(waitFors.size + 1)
      if (i >= waitFors.size) {
        if (state.localStates.nonEmpty && rand.nextDouble() < 0.1) {
          newRandomInvariantCheck(state)
        } else {
          newRandomInvoaction(state)
        }
      } else {
        val (invoc, waitingFor) = waitFors(i)
        makeAction(state, invoc, waitingFor)
      }
    }


    def getPulledTransactions2(state: State, invoc: InvocationId): Set[TransactionId] = {
      val allTransactions = state.transactions.keySet
      val ls: LocalState = state.localStates.getOrElse(invoc, return Set())

      // first: determine how many transactions to base this on:
      var count: Int = 1 + rand.nextInt(2)

      lazy val firstTransaction = !allTransactions.exists(tx => state.transactions(tx).origin == invoc)
      if (rand.nextDouble() < 0.1 && firstTransaction) {
        // some first transactions can be based on no transactions
        count -= 1
      }


      // get transactions, which are not yet in current snapshot
      val pullableTransactions = allTransactions.filter(tx => !ls.isTransactionVisible(tx, state))

      randomSizedSubsetPriority(pullableTransactions, count)
    }

    def makeAction(state: State, invoc: InvocationId, waitingFor: LocalWaitingFor): Action = waitingFor match {
      case WaitForBeginTransaction() =>

        val pulledTransactions1: Set[TransactionId] = getPulledTransactions2(state, invoc)
        //        val pulledTransactions = randomSubset(allTransactions, propability = 0.3)

        // add dependencies =
        var pulledTransactions = pulledTransactions1
        for (tx <- state.transactions.values) {
          if (pulledTransactions1.exists(tx2 => tx.happenedBefore(state.transactions(tx2)))) {
            pulledTransactions += tx.id
          }
        }


        LocalAction(invoc,
          StartTransaction(
            newTransactionId = TransactionId(state.maxTransactionId + 1),
            pulledTransaction = pulledTransactions
          )
        )
      case WaitForFinishInvocation(_) =>
        LocalAction(invoc, Return())
      case WaitForNewId(_, _) =>
        maxGivenId += 1
        LocalAction(invoc, NewId(maxGivenId))
      case WaitForBegin() | WaitForNothing() =>
        throw new RuntimeException("not possible")
    }


    def getPulledTransactions(state: State, allTransactions: Set[TransactionId]): Set[TransactionId] = {
      val transactionsWithSize = for (tx <- allTransactions) yield {
        val size = state.calls.find(_._2.callTransaction == tx) match {
          case Some((cid, cInfo)) => cInfo.callClock.snapshot.size
          case None => 0
        }
        (tx, size)
      }
      val pulledTransactions = randomSubsetW(transactionsWithSize, propability = 0.7)
      pulledTransactions
    }

    def randomValue(typ: InTypeExpr, knownIds: Map[IdType, Map[AnyValue, InvocationId]]): Option[AnyValue] = {
      typ match {
        case SimpleType(name, typeArgs) =>
          def randomDomainValue = {
            // don't generate value '0' as this is the initial value for registers
            Some(Interpreter.domainValue(name, 1 + rand.nextInt(domainSize - 1)))
          }

          prog.findDatatype(name) match {
            case None =>
              randomDomainValue
            case Some(dtU) =>
              val dt = dtU.instantiate(typeArgs)
              if (dt.dataTypeCases.isEmpty)
                randomDomainValue
              else {
                val dtCase = pickRandom(dt.dataTypeCases)(rand)
                val args =
                  for (param <- dtCase.params) yield {
                    randomValue(param.typ, knownIds) match {
                      case Some(v) =>
                        v
                      case None =>
                        return None
                    }
                  }
                Some(AnyValue(DataTypeValue(dtCase.name.name, args)))
              }
          }
        case idt@IdType(_name) =>
          knownIds.get(idt) match {
            case Some(s) =>
              // only pick from the first N (maxUsedIds) unique identifiers to make it more likely that we work on the same data:
              val ids = s.keys.toList.sortBy(_.value.toString).take(maxUsedIds)
              Some(pickRandom(ids)(rand))
            case None => None
          }
        case BoolType() =>
          Some(AnyValue(rand.nextBoolean()))
        case IntType() =>
          Some(AnyValue(rand.nextInt(100)))
        case CallIdType() =>
          ???
        case InvocationIdType() =>
          ???
        case InvocationInfoType() =>
          ???
        case InvocationResultType() =>
          ???
        case SomeOperationType() =>
          ???
        case OperationType(name) =>
          ???
        case FunctionType(argTypes, returnType, source) =>
          ???
        case AnyType() =>
          ???
        case t: TransactionIdType =>
          ???
        case CallInfoType() => ???
        case t: TypeVarUse =>
          throw new RuntimeException(s"Cannot get random value for type variable $t")
        case _: UnitType => Some(AnyValue(()))
      }
    }

    def newRandomInvoaction(state: State): Action = {
      val invocId = InvocationId(state.maxInvocationId + 1)
      var tries = 0
      while (tries < 1000) {
        tries += 1
        var procedures = prog.procedures
        // TODO handle several id types
        //        if (state.knownIds.size >= maxUsedIds) {
        //          // don't call more functions returning id-types
        //          procedures = procedures.filter(p => p.returnType match {
        //            case Some(IdType(name, source)) =>
        //              false
        //            case _ => true
        //          })
        //        }

        val proc = pickRandom(procedures)(rand)

        val args: List[Option[AnyValue]] = for (param <- proc.params) yield {
          randomValue(param.typ, state.knownIds)
        }

        if (!args.contains(None)) {
          return CallAction(invocId, proc.name.name, args.map(_.get))
        }
      }
      // this can happen if all functions require an id-type
      throw new RuntimeException("Could not find suitable arguments for any function")
    }


    def randomSubset[T](set: Set[T], propability: Double = 0.5): Set[T] = {
      set.filter(e => rand.nextDouble() < propability)
    }

    def randomSizedSubset[T](set: Set[T], count: Int): Set[T] = {
      if (set.isEmpty || count <= 0) {
        Set()
      } else {
        val ar = set.toList
        val index = rand.nextInt(ar.size)
        val elem = ar(index)
        randomSubset(set - elem, count - 1) + elem
      }
    }

    def randomElementFromPriorityList[T](list: List[T]): T = list match {
      case Nil => throw new IllegalArgumentException("empty list")
      case List(x) => x
      case (x :: xs) =>
        if (rand.nextDouble() < 0.5) {
          x
        } else {
          randomElementFromPriorityList(xs)
        }
    }

    // random subset with size, but preferring small elements
    def randomSizedSubsetPriority[T](set: Set[T], count: Int)(implicit ordering: Ordering[T]): Set[T] = {
      if (set.isEmpty || count <= 0) {
        Set()
      } else {
        val ar: List[T] = set.toList.sorted(ordering)
        val elem = randomElementFromPriorityList(ar)
        randomSubset(set - elem, count - 1) + elem
      }
    }


    def randomSubsetW[T](set: Set[(T, Int)], propability: Double = 0.5): Set[T] = {
      if (set.isEmpty) {
        return Set()
      }
      val wmax = set.map(_._2).max
      val s = set.filter { case (_, w) =>
        val probability2 = propability * ((1.0 + wmax - w) / wmax)
        rand.nextDouble() < probability2
      }

      return s.map(_._1)
    }

    private def getLocalWaitingFors(state: State): List[(InvocationId, LocalWaitingFor)] = {
      val result = for ((invocation, ls) <- state.localStates) yield {
        (invocation, ls.waitingFor)
      }
      result.toList
    }

  }

  def pickRandom[T](list: List[T])(implicit rand: Random): T = {
    if (list.isEmpty)
      throw new IllegalArgumentException("List empty")
    val i = rand.nextInt(list.size)
    list(i)
  }


  def printTrace(trace: List[Action]): String = {
    val sb = new StringBuilder
    for (action <- trace) {
      sb.append(s"$action\n")
    }
    sb.toString()
  }

  def randomTests(stepsPerRun: Int, numberOfRuns: Int, seed: Int = 0, debug: Boolean = false, timeLimit: Duration, numThreads: Int): Option[QuickcheckCounterexample] = {

    import ExecutionContext.Implicits.global

    val executor = Executors.newFixedThreadPool(numThreads, ConcurrencyUtils.namedThreadFactory("QuickCheck-Test"))


    val tasks = for (i <- 1 to numberOfRuns) yield {
      ConcurrencyUtils.spawnE(
        executor = executor,
        work = { () =>
          LazyList.iterate(i)(_ + numberOfRuns)
            .takeWhile(_ => !Thread.currentThread().isInterrupted)
            .flatMap { seed =>
            randomTestsSingle(stepsPerRun, seed, debug)
          }.headOption
        }
      )
    }

    val timeout = ConcurrencyUtils.runAfter(timeLimit) {
      for (t <- tasks) t.cancel()
    }
    try {
      for (result <- ConcurrencyUtils.race(tasks.toList)) {
        if (result.isDefined) {
          for (t <- tasks) t.cancel()
          return result
        }
      }
      None
    } finally {
      timeout.cancel()
    }
  }

  def randomTestsSingle(limit: Int, seed: Int = 0, debug: Boolean = true): Option[QuickcheckCounterexample] = {
    var startTime = System.nanoTime()

    val ap = new RandomActionProvider(limit, seed)
    try {
      val state = execute(ap)
      val trace = ap.getTrace()

      if (debug) {
        import scala.concurrent.ExecutionContext.Implicits.global
        Future {
          println(s"Executed ${state.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms and found no counterexample.")
          for (action <- trace) {
            debugLog("  " + action)
          }
          Visualization.printStateGraphToFile(prog, state, s"${prog.name}_${seed}_success")
        }
      }
      None
    } catch {
      case e1: InvariantViolationException =>
        val trace = ap.getTrace()

        if (debug) {
          println(s"Found counter example with ${e1.state.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms")
        }

        startTime = System.nanoTime()
        val (smallTrace, smallState, e) = tryShrink(trace, e1.state, e1)

        if (debug) {
          println(s"Reduced to example with ${smallState.invocations.size} invocations in ${(System.nanoTime() - startTime) / 1000000}ms")
          for (action <- smallTrace) {
            debugLog("  " + action)
          }
          debugLog(s"reduced from ${trace.size} to ${smallTrace.size} actions")
          Visualization.printStateGraphToFile(prog, e.state, s"${prog.name}_${seed}_original")
          Visualization.printStateGraphToFile(prog, smallState, s"${prog.name}_${seed}_shrunk")
        }

        val renderResult = Visualization.renderStateGraph(prog, smallState)

        Some(QuickcheckCounterexample(
          brokenInvariant = e.inv.source.range,
          info = e.info,
          state = e.state,
          trace = printTrace(smallTrace)
            + "\n\n" + e.inv.printAst.prettyStr(120)
            + "\n\n" + e.state,
          counterExampleRender = renderResult
        ))
      case e: Throwable =>
        e.printStackTrace()
        throw new RuntimeException("Error in execution: ", e)
    }
  }

  def removeAtIndex[T](l: List[T], index: Int): List[T] =
    l.take(index) ++ l.drop(index + 1)

  // @tailrec TODO why not?
  private def tryShrink(trace: List[Action], lastState: State, lastException: InvariantViolationException): (List[Action], State, InvariantViolationException) = {
    if (Thread.currentThread().isInterrupted) {
      // process cancelled
      return (trace, lastState, lastException)
    }
    debugLog(s"shrinking trace with ${trace.length} elements")
    for (i <- trace.indices) {
      var shrunkTrace = removeAtIndex(trace, i)
      trace(i) match {
        case CallAction(invocationId, _, _) =>
          var hasInvCheck = false
          var snapshot = Set[TransactionId]()
          import scala.util.control.Breaks._
          breakable {
            for (a <- trace) {
              a match {
                case InvariantCheck(id) if id == invocationId =>
                  hasInvCheck = true
                  break
                case LocalAction(id, StartTransaction(txid, pulled)) if id == invocationId =>
                  snapshot = snapshot ++ pulled
                case _ =>
              }
            }
          }
          if (hasInvCheck) {
            // if there is an invariant check in this invocation, try to move it to previous invocations:
            shrunkTrace = shrunkTrace.flatMap {
              case LocalAction(id, StartTransaction(txid, pulled)) if snapshot.contains(txid) =>
                List(
                  LocalAction(id, StartTransaction(txid, pulled)),
                  InvariantCheck(id)
                )
              case a => List(a)
            }
          }
        case _ =>
        // ignore
      }

      debugLog(s"Trying to remove action ${trace(i)}")
      tryShrunkTrace(shrunkTrace) match {
        case Some((tr, e)) =>
          debugLog("Shrinking successful")
          return tryShrink(tr, e.state, e)
        case None =>
          debugLog("Shrinking failed")
        // continue
      }
    }
    (trace, lastState, lastException)
  }

  def tryShrunkTrace(trace: List[Action]): Option[(List[Action], InvariantViolationException)] = {
    val ap = new TraceActionProvider(trace)
    try {
      val state = execute(ap)
      None
    } catch {
      case e: InvariantViolationException =>
        Some((ap.getTrace(), e))
    }
  }


  def execute(actionProvider: ActionProvider): State = {
    var state = State()
    var i = 0
    while (true) {
      i += 1
      val action = actionProvider.nextAction(state)
      debugLog(s"action = $action")
      if (action.isEmpty) {
        return state
      }
      interpreter.executeAction(state, action.get) match {
        case Some(s) =>
          state = s
        case None =>
          actionProvider.discardLast()
      }
    }
    return state
  }


}


object RandomTesterTest {
  def main(args: Array[String]): Unit = {
    //    val input = Helper.getResource("/examples/userbase_fail3.rpls")
    val input = Helper.getResource("/examples/userbase_fail1.rpls")
    //    val input = Helper.getResource("/examples/tournament.rpls")
    val typed = Repliss.parseAndTypecheck("interpretertest", input)
    val prog = typed.get()

    println("prog: ---")
    println(prog.procedures.map(p => s"$p\n${p.body}\n\n").mkString("\n\n"))
    println("-----")


    println("tests start")
    val tester = new RandomTester(prog, RunArgs())
    val result: Option[QuickcheckCounterexample] = tester.randomTests(stepsPerRun = 800, numberOfRuns = 1, timeLimit = Duration.Inf, numThreads = 1)

    result match {
      case Some(ce) =>
        println(s"Counter example:\n$ce")
      case None =>
        println("no counterexample found")

    }

    println("tests done")
  }
}
