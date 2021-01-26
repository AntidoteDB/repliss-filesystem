package crdtver.testing

import java.time.Duration
import java.util
import com.github.peterzeller.logiceval.{NarrowingEvaluator, SimpleEvaluator}
import com.github.peterzeller.logiceval.SimpleLogic.Expr
import crdtver.RunArgs
import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.{Exists, Forall, Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, DataTypeValue, DomainValue, LocalState, State}
import crdtver.utils.MathUtils.{euclideanDiv, euclideanMod}
import crdtver.utils.{MapUtils, MathUtils, PrettyPrintDoc, TimeTaker, myMemo}
import crdtver.utils.PrettyPrintDoc.Doc

import scala.collection.immutable.{::, Nil}


case class Interpreter(val prog: InProgram, runArgs: RunArgs, val domainSize: Int = 3) {

  import Interpreter._

  private val debug: Boolean = false

  def debugLog(s: String): Unit = {
    if (debug) {
      println(s)
    }
  }

  private val timeTaker = new TimeTaker

  def getTimes: Map[String, Duration] = timeTaker.getTimes


  def happensBefore(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    ci1.callClock.happensBefore(ci2.callClock)
  }

  def happensAfter(state: State, c1: CallId, c2: CallId): Boolean = {
    val ci1 = state.calls(c1)
    val ci2 = state.calls(c2)
    ci1.callClock.happensAfter(ci2.callClock)
  }

  def calculatePulledCalls(state: State, visibleCalls: Set[CallId], pulledTransactions: Set[TransactionId]): Set[CallId] = {
    var pulledCalls = Set[CallId]()
    val pulledTransactions2 = pulledTransactions.filter { tr =>
      state.transactions.get(tr) match {
        case Some(trInfo) => trInfo.finished
        case None => false
      }
    }

    // get pulled calls
    for ((c, i) <- state.calls) {
      if (pulledTransactions2.contains(i.callTransaction)) {
        pulledCalls += c
      }
    }
    var calls = visibleCalls
    // get causally dependent calls
    for (c <- pulledCalls) {
      val ci = state.calls(c)
      calls += c
      calls ++= ci.callClock.snapshot
    }
    //    for (c1 <- state.calls.keySet) {
    //      for (c2 <- pulledCalls) {
    //        if (happensBefore(state, c1, c2)) {
    //          calls += c1
    //        }
    //      }
    //    }
    calls
  }


  private def mergeMultimap[K, V](a: Map[K, Set[V]], b: Map[K, Set[V]]): Map[K, Set[V]] = {
    (for (k <- a.keySet ++ b.keySet) yield k -> (a.getOrElse(k, Set()) ++ b.getOrElse(k, Set()))).toMap
  }


  private def mergeKnownIds(a: Map[IdType, Map[AnyValue, InvocationId]], b: Map[IdType, Set[AnyValue]], i: InvocationId): Map[IdType, Map[AnyValue, InvocationId]] = {
    var res = a
    for ((t, vals) <- b) yield {
      var m = a.getOrElse(t, Map())
      for (v <- vals) {
        if (!m.contains(v)) {
          m += v -> i
        }
      }
      res += t -> m
    }
    res
  }

  def executeAction(state: State, action: Action): Option[State] = {
    //    val invocInfo = state.invocations.get(action.invocationId) match {
    //      case Some(invocInfo) => invocInfo.operation.operationName
    //      case None => "new"
    //    }
    //    debugLog(s"execute action $invocInfo $action")
    action match {
      case InvariantCheck(invocationId) =>
        state.localStates.get(invocationId) match {
          case Some(ls) =>
            checkInvariantsLocal(state, ls)
            None
          case None => None
        }
      case CallAction(invocationId, procname, args) =>
        if (state.invocations.contains(invocationId)) {
          // already has invocation with this key
          return None
        }


        val proc: InProcedure = prog.findProcedure(procname)

        if (proc.params.size != args.size) {
          throw new InterepreterException(s"Wrong number of arguments: Calling $procname(${args.mkString(", ")})")
        }

        val varvalues = (for ((param, arg) <- proc.params.zip(args)) yield {
          LocalVar(param.name.name) -> arg
        }).toMap

        val argIds = extractIdsList(args, proc.params.map(_.typ), prog)
        var requiredInvocations: Set[InvocationId] = Set()
        for ((t, ids) <- argIds) {
          val known = state.knownIds.getOrElse(t, Map())
          for (id <- ids) {
            known.get(id) match {
              case Some(invoc) =>
                requiredInvocations += invoc
              case None =>
                // Id is not known yet -> invocation not allowed
                return None
            }
          }
        }

        val requiredTransactions: Set[TransactionId] = for (i <- requiredInvocations; t <- state.transactions.values; if t.origin == i) yield t.id

        val newState = state.copy(
          invocations = state.invocations
            + (invocationId -> InvocationInfo(
            id = invocationId,
            operation = DataTypeValue(procname, args),
            result = None
          )),
          maxInvocationId = state.maxInvocationId.max(invocationId.id),
          localStates = state.localStates + (invocationId ->
            LocalState(
              Some(invocationId),
              varValues = varvalues,
              todo = List(ExecStmt(proc.body)),
              waitingFor = WaitForBegin(),
              currentTransaction = None,
              visibleCalls = calculatePulledCalls(state, Set(), requiredTransactions)
            ))
        )
        Some(executeLocal(invocationId, newState))
      case LocalAction(invocationId, localAction) =>
        val localStateOpt = state.localStates.get(invocationId)
        if (localStateOpt.isEmpty) {
          return None
        }
        val localState = localStateOpt.get
        val waitingFor = localState.waitingFor
        val newState: State = localAction match {
          case StartTransaction(newTransactionId, pulledTransaction) =>
            waitingFor match {
              case WaitForBeginTransaction() =>
                val newVisibleCalls = calculatePulledCalls(state, localState.visibleCalls, pulledTransaction)
                //                debugLog(s"Starting transaction $newTransactionId")
                //                debugLog(s"   old visible = ${localState.visibleCalls}")
                //                debugLog(s"   new visible = $newVisibleCalls")

                val newTransactionInfo = TransactionInfo(
                  id = newTransactionId,
                  origin = invocationId,
                  start = SnapshotTime(newVisibleCalls),
                  currentCalls = List(),
                  finished = false
                )
                val newLocalState: LocalState = localState.copy(
                  visibleCalls = newVisibleCalls,
                  currentTransaction = Some(newTransactionInfo)
                )
                state.copy(
                  localStates = state.localStates + (invocationId -> newLocalState),
                  maxTransactionId = state.maxTransactionId.max(newTransactionId.id)
                )
              case _ =>
                return None
            }
          case Fail() =>
            //            val newInvocationInfo = state.invocations(invocationId).copy(
            //              result = ???
            //            )
            // remove local state
            state.copy(
              localStates = state.localStates - invocationId
              //              invocations = state.invocations + (invocationId -> newInvocationInfo)

            )
          case Return() =>
            waitingFor match {
              case WaitForFinishInvocation(result: AnyValue) =>
                val invocationInfo = state.invocations(action.invocationId)
                val resultDt = DataTypeValue(s"${invocationInfo.operation.operationName}_res", List(result))
                val newInvocationInfo = state.invocations(invocationId).copy(
                  result = Some(resultDt)
                )

                val returnType = prog.findProcedure(newInvocationInfo.operation.operationName).returnType

                val newKnownIds: Map[IdType, Set[AnyValue]] = extractIds(result, returnType, prog)

                // finish current transaction if any
                val state2 = localState.currentTransaction match {
                  case Some(currentTr) =>
                    val tr = currentTr.copy(
                      finished = true
                    )
                    state.copy(
                      calls = state.calls ++ tr.currentCalls.map(c => c.id -> c).toMap,
                      transactions = state.transactions + (tr.id -> tr)
                    )
                  case None =>
                    state
                }


                // remove local state
                return Some(state2.copy(
                  localStates = state.localStates - invocationId,
                  invocations = state.invocations + (invocationId -> newInvocationInfo),
                  knownIds = mergeKnownIds(state.knownIds, newKnownIds, invocationId)
                ))
              case _ =>
                return None
            }
          case NewId(id) =>
            waitingFor match {
              case WaitForNewId(varname, typename) =>
                val generatedValue = AnyValue(DomainValue(typename.name, id))
                val newLocalState = localState.copy(
                  varValues = localState.varValues + (LocalVar(varname) -> generatedValue)
                )
                val oldGenerated: Set[AnyValue] = state.generatedIds.getOrElse(typename, Set())
                val newGenerated: Set[AnyValue] = oldGenerated + generatedValue
                state.copy(
                  localStates = state.localStates + (invocationId -> newLocalState),
                  generatedIds = state.generatedIds + (typename -> newGenerated)
                )
              case _ =>
                return None
            }
        }
        Some(executeLocal(invocationId, newState))
    }

  }

  /**
   * this is like a big-step semantics for local executions
   */
  def executeLocal(invocationId: InvocationId, inState: State): State = {
    var state = inState
    val localState = inState.localStates(invocationId)
    var todo = localState.todo
    var varValues = localState.varValues
    var waitingFor: Option[LocalWaitingFor] = None
    var currentTransaction = localState.currentTransaction
    var visibleCalls = localState.visibleCalls
    while (todo.nonEmpty) {
      val action = todo.head
      todo = todo.tail
      action match {
        case EndAtomic() =>
          var tr = currentTransaction.get
          tr = tr.copy(
            finished = true
          )
          state = state.copy(
            calls = state.calls ++ tr.currentCalls.map(c => c.id -> c).toMap,
            transactions = state.transactions + (tr.id -> tr)
          )
          currentTransaction = None
        case ExecStmt(s) => s match {
          case BlockStmt(source, stmts) =>
            todo = stmts.map(ExecStmt) ++ todo
          case Atomic(source, body) =>
            waitingFor = Some(WaitForBeginTransaction())
            todo = ExecStmt(body) +: EndAtomic() +: todo
            return yieldState()
          case TypedAst.LocalVar(source, variable) =>
          case IfStmt(source, cond, thenStmt, elseStmt) =>
            val condVal = evalExpr(cond, newLocalState(), state)(defaultAnyValueCreator).value
            if (condVal.asInstanceOf[Boolean]) {
              todo = ExecStmt(thenStmt) +: todo
            } else {
              todo = ExecStmt(elseStmt) +: todo
            }
          case MatchStmt(source, expr, cases) =>
            ???
          case CrdtCall(source, FunctionCall(_, _, functionName, _, args, kind)) =>
            val newCallId = CallId(state.maxCallId + 1)
            state = state.copy(maxCallId = newCallId.id)

            visibleCalls = visibleCalls + newCallId
            val newCallInfo: CallInfo = CallInfo(
              id = newCallId,
              operation = DataTypeValue(functionName.name, args.map(evalExpr(_, newLocalState(), state)(defaultAnyValueCreator))),
              callClock = SnapshotTime(visibleCalls),
              callTransaction = currentTransaction.get.id,
              origin = invocationId
            )
            currentTransaction = currentTransaction.map(tr => tr.copy(
              currentCalls = tr.currentCalls :+ newCallInfo
            ))

          case Assignment(source, varname, expr) =>
            val e = evalExpr(expr, newLocalState(), state)(defaultAnyValueCreator)
            varValues = varValues + (LocalVar(varname.name) -> e)
          case NewIdStmt(source, varname, typename) =>
            waitingFor = Some(WaitForNewId(varname.name, typename))
            return yieldState()
          case ReturnStmt(source, expr, assertions) =>
            waitingFor = Some(WaitForFinishInvocation(evalExpr(expr, newLocalState(), state)(defaultAnyValueCreator)))
            return yieldState()
          case a@AssertStmt(source, expr) =>
            val r = evalExpr(expr, localState, state)(defaultAnyValueCreator)
            if (!r.value.asInstanceOf[Boolean])
              throw new AssertionViolatedException(a, state, List())
        }
      }
    }

    // local helper functions
    def newLocalState(): LocalState = LocalState(
      Some(invocationId),
      varValues = varValues,
      todo = todo,
      waitingFor = waitingFor.getOrElse(WaitForNothing()),
      currentTransaction = currentTransaction,
      visibleCalls = visibleCalls
    )

    def yieldState(): State = {
      val ls = newLocalState()
      state.copy(
        localStates = state.localStates + (invocationId -> ls)
      )
    }

    waitingFor = Some(WaitForFinishInvocation(AnyValue("nothing")))
    return yieldState()
  }

  /** checks the invariants in all valid snapshots */
  def checkInvariants(state: State): Unit = {
    timeTaker.measure("invariants") { () =>
      for (inv <- prog.invariants if !inv.isFree) {
        val validSnapshots = TestingHelper.getValidSnapshots(state, state.transactions.values.filter(tx => tx.finished).map(_.id).toSet)
        for (snapshot <- validSnapshots) {
          val visibleCalls =
            (for (tx <- snapshot; c <- state.transactions(tx).currentCalls) yield c.id).toSet
          val localState = LocalState(None, varValues = Map(), todo = List(), waitingFor = WaitForNothing(), None, visibleCalls)
          checkInvariant(state, localState, inv)
        }
      }
    }
  }


  def checkInvariantsLocal(state: State, localState: LocalState): Unit = {
    timeTaker.measure("invariants-local") { () =>
      for (inv <- prog.invariants if !inv.isFree) {
        checkInvariant(state, localState, inv)
      }
    }
  }

  private val invTranslation = new util.IdentityHashMap[InExpr, Expr[Boolean]]()

  private def getLogicEvalExpr(e: InExpr): Expr[Boolean] =
    invTranslation.synchronized {
      var res = invTranslation.get(e)
      if (res == null) {
        res = LogicEvalTranslation.translateExpr(e)(LogicEvalTranslation.Ctxt(prog))
        invTranslation.put(e, res)
      }
      res
    }

  private def checkInvariant(state: State, localState: LocalState, inv: InInvariantDecl): Unit = {
    val lExpr = getLogicEvalExpr(inv.expr)

    val env = LogicEvalTranslation.InterpreterEnv(state, localState, this)

    val r1 = timeTaker.measure(s"${inv.name}_narrowing") { () =>
      try {
        NarrowingEvaluator.startEval(lExpr, env)
      } catch {
        case t: Throwable =>
          throw new Exception(s"When evaluating $lExpr\n${inv.expr}", t)
      }
    }

    if (debug) {
      // in debug mode, also try the other evaluators and check that they return the same result

      val r2 = timeTaker.measure(s"${inv.name}_simple") { () =>
        SimpleEvaluator.startEval(lExpr, env)
      }

      val r3 = timeTaker.measure(s"${inv.name}_old") { () =>
        evalExpr(inv.expr, localState, state)(defaultAnyValueCreator)
      }

      assert(r1 == r2)
      assert(r2 == r3.value)
    }

    if (!r1) {
      val r = evalExpr(inv.expr, localState, state)(tracingAnyValueCreator)
      throw new InvariantViolationException(inv, state, r.info)
    }
  }

  // TODO apply transaction (read own writes)
  def applyTransaction(currentTransaction: Option[TransactionInfo], inState: State): State = inState


  sealed abstract class AnyValueCreator[T <: AbstractAnyValue] {
    def apply(value: Any): T

    def apply(value: AnyValue): T

    def apply(value: Any, other: T): T

    def apply(value: Any, info: => EvalExprInfo, other: T): T = apply(value)

    def getInfo(t: T): List[EvalExprInfo]
  }

  def defaultAnyValueCreator: AnyValueCreator[AnyValue] = new AnyValueCreator[AnyValue] {
    override def apply(value: Any): AnyValue = AnyValue(value)

    override def apply(value: AnyValue): AnyValue = value

    override def apply(value: Any, other: AnyValue): AnyValue = apply(value)

    override def apply(value: Any, info: => EvalExprInfo, other: AnyValue): AnyValue = apply(value)

    override def getInfo(t: AnyValue): List[EvalExprInfo] = List()
  }

  def tracingAnyValueCreator: AnyValueCreator[TracingAnyValue] = new AnyValueCreator[TracingAnyValue] {
    override def apply(value: Any): TracingAnyValue = TracingAnyValue(value)

    override def apply(value: AnyValue): TracingAnyValue = TracingAnyValue(value.value)

    override def apply(value: Any, other: TracingAnyValue): TracingAnyValue = TracingAnyValue(value, other.info)

    override def apply(value: Any, info: => EvalExprInfo, other: TracingAnyValue): TracingAnyValue = TracingAnyValue(value, info :: other.info)

    override def getInfo(t: TracingAnyValue): List[EvalExprInfo] = t.info
  }

  var indent = 0

  def evalExpr[T <: AbstractAnyValue](expr: InExpr, localState: LocalState, inState: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    import PrettyPrintDoc._
    try {
      if (debug) {
        println(nested(4 + indent, (" " * indent) <> "Eval " <> expr.customToString </>
          sep(", ", localState.varValues.toList.map(e => e._1.name <> " -> " <> e._2.toString))))
        indent += 2
      }
      val r = evalExpr2(expr, localState, inState)
      if (debug) {
        indent -= 2
        println((" " * indent) <> "-->" <> r.toString)
      }
      r
    } catch {
      case e: Exception => throw new Exception(s"Error evaluating $expr", e)
    }
  }

  def evalExpr2[T <: AbstractAnyValue](expr: InExpr, localState: LocalState, inState: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    //    debugLog(s"executing expr $expr")
    //    debugLog(s"  vars = ${localState.varValues}")
    val state = applyTransaction(localState.currentTransaction, inState)

    expr match {
      case VarUse(source, typ, name) =>
        val varValue = localState.varValues.get(LocalVar(name)) match {
          case Some(value) =>
            if (value == null)
              throw new RuntimeException(s"Variable $name is null, vars = ${localState.varValues}.")
            value
          case None =>
            throw new RuntimeException(s"Variable $name not bound, vars = ${localState.varValues}.")
        }
        anyValueCreator(varValue)
      case BoolConst(_, _, value) =>
        anyValueCreator(value)
      case IntConst(_, _, value) =>
        anyValueCreator(value)
      case FunctionCall(source, typ, functionName, _, args, kind) =>
        val eArgs: List[T] = args.map(evalExpr(_, localState, state))
        kind match {
          case FunctionKind.FunctionKindDatatypeConstructor() =>
            anyValueCreator(DataTypeValue(functionName.name, eArgs.map(_.toAnyValue)))
          case FunctionKind.FunctionKindCrdtQuery() =>
            evaluateQuery(localState, state, functionName, eArgs)
        }
      case ApplyBuiltin(source, typ, function, args) =>
        val eArgs: Int => T = new myMemo({ i =>
          evalExpr(args(i), localState, state)(anyValueCreator)
        })
        function match {
          case BF_upperBoundedBy() => // TODOO: hardcode
            anyValueCreator(true)
          case BF_isVisible() =>
            anyValueCreator(localState.visibleCalls.contains(eArgs(0).value.asInstanceOf[CallId]))
          case BF_happensBefore(on) =>
            on match {
              case HappensBeforeOn.Unknown() =>
                ???
              case HappensBeforeOn.Call() =>
                val callId1 = eArgs(0).value.asInstanceOf[CallId]
                val callId2 = eArgs(1).value.asInstanceOf[CallId]
                val call1 = state.calls(callId1)
                val call2 = state.calls(callId2)
                anyValueCreator(call1 != call2 && call2.callClock.includes(call1))
              case HappensBeforeOn.Invoc() =>
                val invoc1 = eArgs(0).value.asInstanceOf[InvocationId]
                val invoc2 = eArgs(1).value.asInstanceOf[InvocationId]

                val res: Boolean = happensBeforeInvoc(state, invoc1, invoc2)

                anyValueCreator(res)
            }


          case BF_sameTransaction() =>
            val callId1 = eArgs(0).value.asInstanceOf[CallId]
            val callId2 = eArgs(1).value.asInstanceOf[CallId]
            anyValueCreator(
              state.calls(callId1).callTransaction == state.calls(callId2).callTransaction
            )
          case BF_less() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l < r)

          case BF_lessEq() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l <= r)
          case BF_greater() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l > r)
          case BF_greaterEq() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l >= r)
          case BF_plus() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l + r)
          case BF_minus() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l - r)
          case BF_mult() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(l * r)
          case BF_div() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(euclideanDiv(l, r))
          case BF_mod() =>
            val l = eArgs(0).intValue()
            val r = eArgs(1).intValue()
            anyValueCreator(euclideanMod(l, r))
          case BF_equals() | BF_iff() =>
            //            if (expr.toString.contains("notFound") && !eArgs(0).value.toString.contains("NoResult")) {
            //              debugLog(s"     ${expr}")
            //              debugLog(s"     check ${eArgs(0).value} == ${eArgs(1).value}")
            //            }
            val leftValue: T = eArgs(0)
            val rightValue: T = eArgs(1)
            val comparison: Boolean = leftValue.value == rightValue.value
            anyValueCreator(comparison, EvalEqExprInfo(expr.getSource, leftValue, rightValue), leftValue)
          case BF_notEquals() =>
            anyValueCreator(eArgs(0).value != eArgs(1).value)
          case BF_and() =>
            if (!eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalAndExprInfo(args(0).getSource, Left()), eArgs(0))
            } else if (!eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalAndExprInfo(args(1).getSource, Right()), eArgs(1))
            } else {
              anyValueCreator(true)
            }
          case BF_or() =>
            if (eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalOrExprInfo(args(0).getSource, Left()), eArgs(0))
            } else if (eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalOrExprInfo(args(1).getSource, Right()), eArgs(1))
            } else {
              anyValueCreator(false)
            }
          case BF_implies() =>
            if (!eArgs(0).value.asInstanceOf[Boolean]) {
              anyValueCreator(true, EvalImpliesExprInfo(args(0).getSource, Left()), eArgs(0))
            } else if (!eArgs(1).value.asInstanceOf[Boolean]) {
              anyValueCreator(false, EvalImpliesExprInfo(args(1).getSource, Right()), eArgs(1))
            } else {
              anyValueCreator(true)
            }
          case BF_not() =>
            anyValueCreator(!eArgs(0).value.asInstanceOf[Boolean], eArgs(0))
          case BF_getOperation() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            anyValueCreator(info.operation)
          case BF_getInfo() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            anyValueCreator(info.operation)
          case BF_getResult() =>
            val info: InvocationInfo = state.invocations(eArgs(0).value.asInstanceOf[InvocationId])
            info.result.map(anyValueCreator(_)).getOrElse(anyValueCreator(DataTypeValue("NoResult", List())))
          case BF_getOrigin() =>
            eArgs(0).value match {
              case callId: CallId =>
                val info = state.calls(callId)
                anyValueCreator(info.origin)
              case txId: TransactionId =>
                val tx = state.transactions(txId)
                anyValueCreator(tx.origin)
            }
          case BF_getTransaction() =>
            val info: CallInfo = state.calls(eArgs(0).value.asInstanceOf[CallId])
            anyValueCreator(info.callTransaction)
          case BF_inCurrentInvoc() =>
            ???
          case BF_distinct() =>
            val eArgsList = args.indices.map(eArgs)
            for ((a, i) <- eArgsList.zipWithIndex)
              for (j <- i + 1 until args.size)
                if (a.value == eArgs(j).value)
                  return anyValueCreator(false)
            anyValueCreator(true)
        }

      case QuantifierExpr(source, Exists(), vars, e) =>

        // find variable assignment making this true
        val results = (for (m <- enumerate(vars, state)) yield {
          val newLocalState = localState.copy(varValues = localState.varValues ++ m)
          val r = evalExpr(e, newLocalState, state)(anyValueCreator)
          if (r.value.asInstanceOf[Boolean]) {
            return anyValueCreator(true, QuantifierInfo(source, m), r)
          }
          (m, r)
        }).force
        // no matching value exists
        anyValueCreator(false, QuantifierAllInfo(source, results.map(e => (e._1, anyValueCreator.getInfo(e._2))).toList, result = false), anyValueCreator(false))
      //        var res = anyValueCreator(false)
      //        res match {
      //          case tres: TracingAnyValue =>
      //          val infos = tres.info
      //            for ((m,r) <- results) {
      //              res = anyValueCreator(false, QuantifierAllInfo(source, m, r.asInstanceOf[TracingAnyValue].info), res)
      //            }
      //          case _ =>
      //        }
      //        return res
      case QuantifierExpr(source, Forall(), vars, e) =>

        // find variable assignment making this true
        val results = (for (m <- enumerate(vars, state)) yield {
          val newLocalState = localState.copy(varValues = localState.varValues ++ m)
          val r = evalExpr(e, newLocalState, state)(anyValueCreator)
          if (!r.value.asInstanceOf[Boolean]) {
            return anyValueCreator(false, QuantifierInfo(source, m), r)
          }
          (m, r)
        }).force

        // all values matching
        anyValueCreator(true)
      //        var res = anyValueCreator(true)
      //        res match {
      //          case tres: TracingAnyValue =>
      //            val infos = tres.info
      //            for ((m,r) <- results) {
      //              res = anyValueCreator(false, QuantifierAllInfo(source, m, r.asInstanceOf[TracingAnyValue].info), res)
      //            }
      //          case _ =>
      //        }
      //        return res
      case AggregateExpr(source, op, vars, filter, elem) =>
        val elems: LazyList[BigInt] =
          for (m <- enumerate(vars, state)) yield {
            val newLocalState = localState.copy(varValues = localState.varValues ++ m)
            val f = evalExpr(filter, newLocalState, state)(anyValueCreator)
            if (f.value.asInstanceOf[Boolean]) {
              evalExpr(elem, newLocalState, state)(anyValueCreator).intValue()
            } else {
              0
            }
          }
        anyValueCreator(elems.sum)
      case InAllValidSnapshots(_, e) =>
        // not relevant for interpreter?
        ???
      case CrdtQuery(source, typ, qryOp) =>
        // not relevant for interpreter?
        throw new RuntimeException(s"Could not evaluate query $qryOp")
    }
  }

  def happensBeforeInvoc(state: State, invoc1: InvocationId, invoc2: InvocationId): Boolean = {
    var calls1 = Set[CallInfo]()
    var calls2 = Set[CallInfo]()

    for ((id, c) <- state.calls) {
      if (c.origin == invoc1)
        calls1 += c
      if (c.origin == invoc2)
        calls2 += c
    }
    val res =
      invoc1 != invoc2 &&
        calls1.nonEmpty &&
        calls2.nonEmpty &&
        calls1.forall(c1 => calls2.forall(c2 => c1.happensBefore(c2)))
    res
  }

  def evaluateQueryOp(localState: LocalState, state: State, qryOp: DataTypeValue): Any = {
    evaluateQuery(localState, state, Identifier(NoSource(), qryOp.operationName), qryOp.args)(defaultAnyValueCreator).value
  }

  def evaluateQuery[T <: AbstractAnyValue](localState: LocalState, state: State, functionName: Identifier, eArgs: List[T])(implicit anyValueCreator: AnyValueCreator[T]): T = {
    require(prog.hasQuery(functionName.name))
    timeTaker.measure(s"evaluate ${functionName.name}") { () =>
      val visibleState = state.copy(
        calls = state.calls.filter { case (c, ci) => localState.visibleCalls.contains(c) }
      )

      prog.programCrdt.evaluateQuery(functionName.name, eArgs, visibleState) match {
        case Some(res) =>
          anyValueCreator(res)
        case None =>
          prog.findQuery(functionName.name) match {
            case Some(query) =>
              evaluateQueryDecl(query, eArgs, localState, state)
            case None =>
              anyValueCreator(DataTypeValue(functionName.name, eArgs.map(a => AnyValue(a.value))))
          }
      }
    }
  }

  def evaluateQueryDecl[T <: AbstractAnyValue](query: InQueryDecl, eArgs: List[T], localState: LocalState, state: State)(implicit anyValueCreator: AnyValueCreator[T]): T = {
    evaluateQueryDeclLazyList(query, eArgs, localState, state).headOption.getOrElse(anyValueCreator("invalid"))
  }

  def evaluateQueryDeclLazyList[T <: AbstractAnyValue](query: InQueryDecl, eArgs: List[T], localState: LocalState, state: State)(implicit anyValueCreator: AnyValueCreator[T]): LazyList[T] = {
    val paramValues: Map[LocalVar, AnyValue] = query.params.zip(eArgs).map {
      case (param, value) => LocalVar(param.name.name) -> AnyValue(value.value)
    }.toMap

    val ls = localState.copy(
      varValues = paramValues
    )

    query.implementation match {
      case Some(impl) =>
        LazyList(evalExpr(impl, ls, state))
      case None =>
        query.ensures match {
          case Some(ensures) =>

            // try to find valid value
            val validValues = enumerateValues(query.returnType, state).filter(r => {
              val ls2 = ls.copy(
                varValues = ls.varValues + (LocalVar("result") -> r)
              )
              val check: T = evalExpr(ensures, ls2, state)
              check.value.asInstanceOf[Boolean]
            })
            // return first matching value or "invalid" if postcondition cannot be satisfied (should not happen for valid postconditions)
            validValues.map(anyValueCreator(_))
          case None =>
            // this is just a dummy implementation returning an arbitrary result
            //                enumerateValues(query.returnType, state).head
            LazyList.empty
        }

    }
  }

  def enumerate(vars: List[InVariable], state: State): LazyList[Map[LocalVar, AnyValue]] = vars match {
    case Nil =>
      List(Map[LocalVar, AnyValue]()).to(LazyList)
    case List(v) =>
      enumerateValues(v.typ, state).map(x => Map(LocalVar(v.name.name) -> x))
    case v :: tl =>
      val s = enumerateValues(v.typ, state).map(x => Map(LocalVar(v.name.name) -> x))
      s.flatMap(vals => {
        val tls = enumerate(tl, state)
        tls.map(vals2 => {
          vals ++ vals2
        })
      })
  }

  def enumerateValues(t: InTypeExpr, state: State): LazyList[AnyValue] = t match {
    case AnyType() =>
      ???
    case BoolType() =>
      AnyValue(true) #:: AnyValue(false) #:: LazyList.empty
    case IntType() =>
      ???
    case CallIdType() =>
      state.calls.keys.to(LazyList).map(AnyValue(_))
    case InvocationIdType() =>
      state.invocations.keys.to(LazyList).map(AnyValue(_))
    case TransactionIdType() =>
      state.transactions.keys.to(LazyList).map(AnyValue(_))
    case InvocationInfoType() =>
      ???
    case InvocationResultType() =>
      ???
    case SomeOperationType() =>
      ???
    case OperationType(name) =>
      ???
    case FunctionType(argTypes, returnType, kind) =>
      ???
    case SimpleType(name, typeArgs) =>
      this.prog.findDatatype(name) match {
        case None =>
          customTypeDomain(name)
        case Some(dt1) =>
          val dt = dt1.instantiate(typeArgs)
          for (dtcase <- dt.dataTypeCases.to(LazyList); params <- enumerate(dtcase.params, state)) yield {
            val args = dtcase.params.map(p => params(LocalVar(p.name.name)))
            AnyValue(DataTypeValue(dtcase.name.name, args))
          }
      }
    case idt@IdType(name) =>
      state.generatedIds.getOrElse(idt, Set()).to(LazyList)
    case CallInfoType() => ???
    case t: TypeVarUse =>
      throw new RuntimeException(s"Cannot enumerate type variable $t")
    case _: UnitType => LazyList(AnyValue(()))
  }

  private def collectCustomValues(TypeName: String)(p: AnyValue): Set[DomainValue] = p match {
    case AnyValue(DataTypeValue(_, args)) =>
      args.flatMap(collectCustomValues(TypeName)).toSet
    case AnyValue(dv@DomainValue(TypeName, i)) => Set(dv)
    case _ => Set()
  }

  /** Like enumerateValues, but checks the custom values used in invocations and returns
   * only the values that are already used in invocations plus one additional new value
   *
   * this is to optimize the explored space
   */
  def enumerateNewValues(typ: InTypeExpr, state: State): LazyList[AnyValue] = {
    typ match {
      case s: SimpleType =>
        this.prog.findDatatype(s.name) match {
          case None =>
            // collect all used so far
            val used: Set[DomainValue] = state.invocations
              .flatMap(i => i._2.operation.args.flatMap(collectCustomValues(s.name))).toSet
            val maxId = used.map(_.i).maxOption.getOrElse(0)
            return used.to(LazyList).map(AnyValue) ++
              (if (maxId + 1 < domainSize)
                LazyList(AnyValue(DomainValue(s.name, maxId + 1)))
              else if (used.contains(DomainValue(s.name, 0)))
                LazyList()
              else
                LazyList(AnyValue(DomainValue(s.name, 0))))
          case _ =>
        }
      case _ =>
    }
    enumerateValues(typ, state)
  }


  def customTypeDomain(name: String): LazyList[AnyValue] = {
    (0 until domainSize).map(i => domainValue(name, i)).to(LazyList)
  }

  class InterepreterException(msg: String, cause: Throwable = null) extends Exception(msg, cause)

}

object Interpreter {

  def defaultValue(t: InTypeExpr, state: State, interpreter: Interpreter): AnyValue = t match {
    case BoolType() => AnyValue(false)
    case IntType() => AnyValue(0)
    case _ => interpreter.enumerateValues(t, state).headOption.getOrElse(AnyValue(s"default<$t>"))
  }


  class InvariantViolationException(val inv: InInvariantDecl, val state: State, val info: List[EvalExprInfo])
    extends RuntimeException(s"Invariant in line ${inv.source.getLine}") {
  }

  abstract class AbstractAnyValue {
    def value: Any

    /** removes any extra information */
    def toAnyValue: AnyValue = AnyValue(value)

    def intValue(): BigInt = value match {
      case i: BigInt => i
      case _ => throw new RuntimeException(s"Value $value of type ${value.getClass} cannot be cast to Int.")
    }

  }

  case class AnyValue(value: Any) extends AbstractAnyValue {
    require(!value.isInstanceOf[AnyValue])

    override def toString: String = value.toString
  }

  case class TracingAnyValue(value: Any, info: List[EvalExprInfo] = List()) extends AbstractAnyValue {
    override def toString: String = value.toString
  }

  case class TransactionId(id: Int) {
    override def toString: String = s"tx_$id"
  }

  object TransactionId {
    implicit def orderById: Ordering[TransactionId] = Ordering.by(_.id)
  }


  case class CallId(id: Int) {
    override def toString: String = s"call_$id"
  }

  object CallId {
    implicit val ord: Ordering[CallId] = Ordering.by(_.id)
  }


  case class InvocationId(id: Int) {
    override def toString: String = s"invoc_$id"
  }

  object InvocationId {
    implicit val ord: Ordering[InvocationId] = Ordering.by(_.id)
  }


  case class DataTypeValue(operationName: String, args: List[AnyValue]) {
    override def toString: String = s"$operationName(${args.mkString(", ")})"
  }

  case class CallInfo(
    id: CallId,
    operation: DataTypeValue,
    callClock: SnapshotTime,
    callTransaction: TransactionId,
    origin: InvocationId
  ) {
    def happensBefore(c2: CallInfo): Boolean = c2.callClock.snapshot.contains(id)

    def happensAfter(c2: CallInfo): Boolean = this.callClock.snapshot.contains(c2.id)

    def happensParallel(c2: CallInfo): Boolean = !happensAfter(c2) && !happensBefore(c2)
  }

  case class SnapshotTime(snapshot: Set[CallId]) {
    def includes(call: CallInfo): Boolean = {
      snapshot.contains(call.id)
    }

    def happensBefore(other: SnapshotTime): Boolean = {
      SnapshotTimeOrder.lteq(this, other)
    }

    def happensAfter(other: SnapshotTime): Boolean = {
      SnapshotTimeOrder.gteq(this, other)
    }
  }

  implicit object SnapshotTimeOrder extends PartialOrdering[SnapshotTime] {
    override def tryCompare(x: SnapshotTime, y: SnapshotTime): Option[Int] = {
      if (x.snapshot == y.snapshot) {
        Some(0)
      } else if (x.snapshot.subsetOf(y.snapshot)) {
        Some(-1)
      } else if (y.snapshot.subsetOf(x.snapshot)) {
        Some(1)
      } else {
        None
      }
    }

    override def lteq(x: SnapshotTime, y: SnapshotTime): Boolean = {
      x.snapshot.subsetOf(y.snapshot)
    }
  }


  case class TransactionInfo(
    id: TransactionId,
    start: SnapshotTime,
    origin: InvocationId,
    currentCalls: List[CallInfo],
    finished: Boolean
  ) {
    def happenedBefore(other: TransactionInfo): Boolean = {
      currentCalls.forall(c => other.start.snapshot.contains(c.id))
    }

  }

  case class DomainValue(name: String, i: Int) {
    override def toString: String = s"${name}_$i"
  }

  def domainValue(name: String, i: Int): AnyValue = {
    AnyValue(DomainValue(name, i))
  }


  case class InvocationInfo(
    id: InvocationId,
    operation: DataTypeValue,
    result: Option[DataTypeValue]
  )


  case class LocalVar(name: String) {
    override def toString: String = name
  }

  // local state for one invocation
  case class LocalState(
    currentInvoc: Option[InvocationId],
    varValues: Map[LocalVar, AnyValue],
    todo: List[StatementOrAction],
    waitingFor: LocalWaitingFor,
    currentTransaction: Option[TransactionInfo],
    visibleCalls: Set[CallId]
  ) {

    def isCallVisible(c: CallId): Boolean = {
      return visibleCalls.contains(c)
    }

    def isTransactionVisible(tx: TransactionId, state: State): Boolean = {
      visibleCalls.exists(c => state.calls(c).callTransaction == tx)
    }

    def toDoc: PrettyPrintDoc.Doc = {
      import crdtver.utils.PrettyPrintDoc._


      "LocalState(" <> nested(2, line <>
        "varValues:" </>
        sep(line, varValues.toList.map { case (k, v) => k.name <> " -> " <> v.toString }) </>
        "todo: " <> todo.size.toString </>
        "waitingFor: " <> waitingFor.toString </>
        "currentTransaction: " <> currentTransaction.toString </>
        "visibleCalls: " <> visibleCalls.toString() </>
        ")")
    }

    override def toString: String =
      s"""
         |LocalState(
         |  varValues:
         |    ${varValues.toList.map { case (k, v) => s"$k -> $v" }.mkString("\n    ")}
         |  todo: ${todo.size}
         |  waitingFor: $waitingFor
         |  currentTransaction: $currentTransaction
         |  visibleCalls: $visibleCalls
         |)
       """.stripMargin
  }

  sealed abstract class LocalWaitingFor

  case class WaitForBegin() extends LocalWaitingFor

  case class WaitForNothing() extends LocalWaitingFor

  case class WaitForBeginTransaction() extends LocalWaitingFor

  case class WaitForFinishInvocation(result: AnyValue) extends LocalWaitingFor

  case class WaitForNewId(varname: String, typename: IdType) extends LocalWaitingFor


  sealed abstract class StatementOrAction

  case class ExecStmt(inStatement: InStatement) extends StatementOrAction

  case class EndAtomic() extends StatementOrAction


  // System state
  case class State(
    // the set of all calls which happened on the database
    calls: Map[CallId, CallInfo] = Map(),
    //    transactionCalls: Map[TransactionId, Set[CallId]] = Map(),
    maxCallId: Int = 0,
    transactions: Map[TransactionId, TransactionInfo] = Map(),
    maxTransactionId: Int = 0,
    invocations: Map[InvocationId, InvocationInfo] = Map(),
    maxInvocationId: Int = 0,
    // returned Ids for each id-type and the invocation that returned it
    knownIds: Map[IdType, Map[AnyValue, InvocationId]] = Map(),
    generatedIds: Map[IdType, Set[AnyValue]] = Map(),
    localStates: Map[InvocationId, LocalState] = Map()

  ) {
    // only for faster evaluation:
    lazy val operationToCall: Map[DataTypeValue, CallId] =
      calls.values.map(ci => (ci.operation, ci.id)).toMap

    def toDoc: Doc = {
      import crdtver.utils.PrettyPrintDoc._


      "state" <> nested(2, line <>
        "calls: " <> nested(2, line <> sep(line, calls.values.map(c => c.toString))) </>
        "maxCallId: " <> maxCallId.toString </>
        "transactions: " <> nested(2, line <> sep(line, transactions.values.map(c => c.toString))) </>
        "maxTransactionId: " <> maxTransactionId.toString </>
        "invocations: " <> nested(2, line <> sep(line, invocations.values.map(c => c.toString))) </>
        "maxInvocationId: " <> maxInvocationId.toString </>
        "knownIds: " <> knownIds.toString </>
        "localStates: " <> nested(2, line <> sep(line, localStates.map(c => c._1.toString <> " -> " <> c._2.toDoc)))
      )
    }

    override def toString: String =
      toDoc.prettyStr(140)
  }


  // actions taken by the interpreter
  sealed trait Action {
    def print: String = this match {
      case CallAction(invocationId, procname, args) =>
        s"$invocationId call $procname(${args.mkString(", ")})"
      case LocalAction(invocationId, localAction) =>
        s"$invocationId    " + (localAction match {
          case StartTransaction(newTransactionId, pulledTransaction) =>
            s"startTx(${pulledTransaction.mkString(", ")}) => $newTransactionId"
          case Fail() =>
            "fail"
          case Return() =>
            "return"
          case NewId(id) =>
            s"newId($id)"
        })
      case InvariantCheck(invocationId) =>
        s"$invocationId    invCheck"
    }

    val invocationId: InvocationId
  }

  case class CallAction(
    invocationId: InvocationId,
    procname: String,
    args: List[AnyValue]
  ) extends Action

  case class LocalAction(
    invocationId: InvocationId,
    localAction: LocalStep
  ) extends Action

  case class InvariantCheck(
    invocationId: InvocationId
  ) extends Action


  sealed trait LocalStep

  case class StartTransaction(
    newTransactionId: TransactionId,
    pulledTransaction: Set[TransactionId]
  ) extends LocalStep

  case class Fail() extends LocalStep

  case class Return() extends LocalStep

  case class NewId(id: Int) extends LocalStep

  abstract class EvalExprInfo {
    override final def toString: String =
      doc.prettyStr(140)

    def doc: Doc
  }

  import PrettyPrintDoc._

  def vars(info: Map[LocalVar, AnyValue]): List[String] =
    for ((k, v) <- info.toList) yield s"$k ↦ $v"

  case class QuantifierInfo(source: SourceTrace, info: Map[LocalVar, AnyValue]) extends EvalExprInfo {
    override def doc: Doc = {
      "Quantifier in line " <> source.getLine.toString <>
        " instantiated with " <> vars(info).mkString(", ")
    }
  }

  case class QuantifierAllInfo(source: SourceTrace, info: List[(Map[LocalVar, AnyValue], List[EvalExprInfo])], result: Boolean) extends EvalExprInfo {
    override def doc: Doc = {
      "Quantifier in line " <> source.getLine.toString <> " evaluated to " <> result.toString <> " with the following sub-queries:" <>
        nested(4, line <> info.map(e => "When instantiated with " <> vars(e._1).mkString(", ") <> ":" <> nested(4, e._2.map(x => line <> x.doc)) <> line))
    }
  }

  sealed abstract class Side

  case class Left() extends Side {
    override def toString: String = "left"
  }

  case class Right() extends Side {
    override def toString: String = "right"
  }

  case class EvalAndExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def doc: Doc = {
      s"Conjunction in line ${source.getLine} was false on the $side side"
    }
  }

  case class EvalOrExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def doc: Doc = {
      s"Disjunction in line ${source.getLine} was true on the $side side"
    }
  }

  case class EvalImpliesExprInfo(source: SourceTrace, side: Side) extends EvalExprInfo {
    override def doc: Doc = side match {
      case Left() => s"Assumption of implication in line ${source.getLine} was false"
      case Right() => s"Right side of implication in line ${source.getLine} was false"
    }
  }

  case class EvalEqExprInfo(source: SourceTrace, left: AbstractAnyValue, right: AbstractAnyValue) extends EvalExprInfo {
    override def doc: Doc = {
      s"Equality check was false: $left != $right"
    }
  }


  def extractIdsList(args: List[AnyValue], argTypes: List[InTypeExpr], prog: InProgram): Map[IdType, Set[AnyValue]] = {
    val idList = args.zip(argTypes).map(a => extractIds(a._1, a._2, prog))
    idList.fold(Map())(_ ++ _)
  }

  def extractIds(result: AnyValue, returnType: InTypeExpr, prog: InProgram): Map[IdType, Set[AnyValue]] = returnType match {
    case idt@IdType(name) =>
      Map(idt -> Set(result))
    case SimpleType(name, typeArgs) =>
      prog.findDatatype(name) match {
        case Some(dt1) =>
          val dt = dt1.instantiate(typeArgs)
          result.value match {
            case dval: DataTypeValue =>
              val c = dt.dataTypeCases.find(_.name.name == dval.operationName).get
              extractIdsList(dval.args, c.params.map(_.typ), prog)
            case other =>
              throw new Exception(s"Cannot extract ids from ${other} (${other.getClass})")
          }
        case None =>
          Map()
      }
    case _ =>
      // TODO handle datatypes with nested ids
      Map()
  }

}
