package crdtver.symbolic

import java.util.concurrent.TimeUnit

import crdtver.language.InputAst
import crdtver.language.InputAst.{IdType, InProgram, SourceRange, SourceTrace}
import crdtver.symbolic.IsabelleTranslation.createIsabelleDefs
import crdtver.symbolic.SVal._
import crdtver.symbolic.SymbolicContext._
import crdtver.symbolic.SymbolicMapVar.symbolicMapVar
import crdtver.symbolic.SymbolicSort._
import crdtver.testing.Interpreter.{AnyValue, CallId, CallInfo, DataTypeValue, InvocationId, InvocationInfo, SnapshotTime, TransactionId, TransactionInfo}
import crdtver.testing.{Interpreter, Visualization}
import crdtver.utils.PrettyPrintDoc.Doc
import crdtver.utils.StringBasedIdGenerator

import scala.concurrent.duration.Duration
import scala.language.implicitConversions

case class SymbolicExecutionRes(
  proc: String,
  time: Duration,
  error: Option[SymbolicCounterExample]
)

class SymbolicExecutionException(
  val counterExample: SymbolicCounterExample
) extends RuntimeException(counterExample.message) {
  override def toString: String = s"SymbolicExecutionError:\n$counterExample"
}

class SymbolicEvaluator(
  val prog: InProgram
) {


  def checkProgram(): Stream[SymbolicExecutionRes] = {
    debugPrint("checking program")
    for (proc <- prog.procedures.toStream) yield checkProcedure(proc)
  }

  private def idTypes(): List[InputAst.InTypeDecl] =
    prog.types.filter(_.isIdType)


  private def checkProcedure(proc: InputAst.InProcedure): SymbolicExecutionRes = {
    val startTime = System.currentTimeMillis()
    try {
      debugPrint(s"checking procedure ${proc.name}")
      val z3Translation: SmtTranslation = new Cvc4Translation()
      implicit val ctxt: SymbolicContext = new SymbolicContext(z3Translation, proc.name.name, prog)
      z3Translation.datatypeImpl = ctxt.datypeImpl

      val params = makeVariablesForParameters(ctxt, proc.params)

      val generatedIds: Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] =
        generatedIdsVar(ctxt)
      val knownIds: Map[IdType, SVal[SortSet[SortCustomUninterpreted]]] =
        knownIdsVar(ctxt)


      // in the beginning everything is unknown so we use symbolic variables:
      val state: SymbolicState = SymbolicState(
        calls = symbolicMapVar("calls"),
        //[SortCallId, SortOption[SortCall], Nothing](ctxt.makeVariable("calls"))(default()),
        happensBefore = symbolicMapVar("happensBefore"),
        callOrigin = symbolicMapVar("callOrigin"),
        transactionOrigin = symbolicMapVar("transactionOrigin"),
        transactionStatus = symbolicMapVar("transactionStatus"),
        generatedIds = generatedIds,
        knownIds = knownIds,
        invocationCalls = symbolicMapVar("invocationCalls"),
        invocationOp = symbolicMapVar("invocationOp"),
        invocationRes = symbolicMapVar("invocationRes"),
        currentInvocation = ctxt.makeVariable("currentInvocation"),
        localState = params.toMap,
        visibleCalls = SSetEmpty(),
        trace = Trace()
      )

      // there are a few restrictions we can assume for the initial state:
      // this follows the begin-invoc rule

      // >> invocationOp S i = None;
      ctxt.addConstraint("i_fresh",
        SMapGet(state.invocationOp, state.currentInvocation) === SInvocationInfoNone())

      // >> procedure (prog S) procName args ≜ (initState, impl);
      // >>   uniqueIdsInList args ⊆ knownIds S';
      // >>   state_wellFormed S';
      // >>   ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
      val var_tx = ctxt.makeVariable[SortTxId]("tx")

      ctxt.addConstraint("no_uncommitted_transactions",
        forall(var_tx, state.transactionStatus(var_tx) !== SSome(SUncommitted())))

      // >>   invariant_all S';
      ctxt.addConstraint("invariant_pre",
        invariant(state)(ctxt))
      // >>   invocationOp S' i = None;
      val i = state.currentInvocation

      // >>   prog S' = prog S;
      // >>   S'' = (S'⦇localState := (localState S')(i ↦ initState),
      // this is handled by makeVariablesForParameters
      // >>   currentProc := (currentProc S')(i ↦ impl),
      // see proc
      // >>   visibleCalls := (visibleCalls S')(i ↦ {}),
      // see state.visibleCalls
      // >>   invocationOp := (invocationOp S')(i ↦ (procName, args)) ⦈);
      // >>   ⋀tx. transactionOrigin S'' tx ≠ Some i

      // there are no calls in the current invocation:
      ctxt.addConstraint("no_call_in_new_invocation",
        state.invocationCalls.get(i) === SSetEmpty())

      val args: List[SVal[SortValue]] = params.map(_._2)
      // >>   valid = invariant_all S'';  ― ‹  TODO check invariant in C ?  ›
      val invocationInfo: SVal[SortInvocationInfo] = SInvocationInfo(proc.name.name, args)

      val state2 = state.copy(
        invocationOp = state.invocationOp.put(i, invocationInfo),
        //SymbolicMapUpdated(i, invocationInfo, state.invocationOp)
      ).withTrace(s"Invocation of ${proc.name}(${args.mkString(", ")})", proc.getSource())

      // check the invariant in state2:
      checkInvariant(proc.getSource(), ctxt, state2, s"directly after invocation of ${proc.name}") match {
        case Some(msg) =>
          throw new SymbolicExecutionException(msg)
        case None =>
        // ok
      }

      // continue evaluating the procedure body:
      executeStatement(proc.body, state2, ctxt, (s, _) => s)
      SymbolicExecutionRes(
        proc.name.name,
        Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
        None
      )
    } catch {
      case e: SymbolicExecutionException =>
        SymbolicExecutionRes(
          proc.name.name,
          Duration.apply(System.currentTimeMillis() - startTime, TimeUnit.MILLISECONDS),
          Some(e.counterExample)
        )
    }


  }


  private def knownIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicVariable[SortSet[SortCustomUninterpreted]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val sort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> ctxt.makeVariable[SortSet[SortCustomUninterpreted]](s"knownIds_${t.name}")(SortSet(sort))
    })
      .toMap
  }

  private def generatedIdsVar(implicit ctxt: SymbolicContext): Map[IdType, SymbolicMap[SortCustomUninterpreted, SortOption[SortInvocationId]]] = {
    idTypes().map(t => {
      val idType = IdType(t.name.name)()
      val keySort: SortCustomUninterpreted = ctxt.translateSortCustomUninterpreted(idType)
      idType -> symbolicMapVar[SortCustomUninterpreted, SortOption[SortInvocationId]](s"generatedIds_${t.name}")(keySort, implicitly, implicitly)
    })
      .toMap
  }

  def executeStatements(stmts: List[InputAst.InStatement], state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = stmts match {
    case Nil =>
      follow(state, ctxt)
    case x :: xs =>
      executeStatement(x, state, ctxt, executeStatements(xs, _, _, follow))
  }


  /**
    * Asserts that the state is wellformed
    */
  def wellFormed(state: SymbolicState): SVal[SortBoolean] =
    SBool(true)

  def debugPrint(str: => String): Unit = {
    //    debugPrint(str)
  }

  private def executeStatement(stmt: InputAst.InStatement, state: SymbolicState, ctxt: SymbolicContext, follow: (SymbolicState, SymbolicContext) => SymbolicState): SymbolicState = {
    implicit val istate: SymbolicState = state
    stmt match {
      case InputAst.BlockStmt(source, stmts) =>
        debugPrint(s"Executing block in line ${source.getLine}")
        executeStatements(stmts, state, ctxt, follow)
      case InputAst.Atomic(source, body) =>
        debugPrint(s"Executing begin-atomic in line ${source.getLine}")
        val state2 = executeBeginAtomic(source, state, ctxt)
        executeStatement(body, state2, ctxt, (state3, ctxt) => {
          debugPrint(s"Executing end-atomic in line ${source.range.stop.line}")
          val state4 = executeEndAtomic(state3, ctxt)

          // assume state 4 wellformed
          ctxt.addConstraint("wellformed_after_transaction", wellFormed(state4))
          // check invariant in state4
          checkInvariant(source, ctxt, state4, s"When committing transaction of atomic block in line ${source.getLine}.") match {
            case Some(msg) =>
              throw new SymbolicExecutionException(msg)
            case None =>
            // ok
          }

          follow(state4, ctxt)
        })
      case InputAst.LocalVar(source, variable) =>
        debugPrint(s"Executing local variable $variable in line ${source.getLine}")
        // nothing to do
        state.withTrace(s"Local variable $variable", variable.getSource())
      case InputAst.IfStmt(source, cond, thenStmt, elseStmt) =>
        debugPrint(s"Executing if-statement in line ${source.getLine}")
        val condV: SVal[SortBoolean] = ExprTranslation.translate(cond)(bool, ctxt, state)
        ctxt.inContext(() => {
          // first assume the condition is true
          ctxt.addConstraint("if_statement_condition_true", condV)
          ctxt.check() match {
            case Unsatisfiable =>
            // then-branch cannot be taken
            case Unknown | _: Satisfiable =>
              debugPrint(s"Executing then-statement in line ${thenStmt.getSource().getLine}")
              val state2 = state.withTrace(s"Executing then-statement", thenStmt)
              executeStatement(thenStmt, state2, ctxt, follow)
          }
        })
        // next assume the condition is false:
        debugPrint(s"Executing else-statement in line ${elseStmt.getSource().getLine}")
        ctxt.addConstraint("if_statement_condition_false", SNot(condV))
        ctxt.check() match {
          case Unsatisfiable =>
            // else-branch cannot be taken
            state.copy(satisfiable = false)
          case Unknown | _: Satisfiable =>
            val state2 = state.withTrace("Executing else-statement", elseStmt)
            executeStatement(elseStmt, state2, ctxt, follow)
        }
      case InputAst.MatchStmt(source, expr, cases) =>
        // TODO
        ???
      case InputAst.CrdtCall(source, call) =>
        debugPrint(s"Executing CRDT call in line ${source.getLine}")
        val t: SVal[SortTxId] = state.currentTransaction.get
        val c: SymbolicVariable[SortCallId] = ctxt.makeVariable("c" + state.currentCallIds.size)
        // assume new call c is distinct from all others:
        ctxt.addConstraint(s"${c.name}_freshA", SDistinct(c :: state.currentCallIds))
        ctxt.addConstraint(s"${c.name}_freshB", SEq(state.calls.get(c), SCallInfoNone()))

        // TODO maybe choose type based on query type
        // TODO assume query specification for res

        val args: List[SVal[SymbolicSort]] = call.args.map(a => ExprTranslation.translateUntyped(a)(ctxt, state))
        val callInfo: SVal[SortCall] = SCallInfo(call.functionName.name, args)

        val state2 = state.copy(
          calls = state.calls.put(c, callInfo),
          currentCallIds = state.currentCallIds :+ c,
          callOrigin = state.callOrigin.put(c, SSome(t)),
          visibleCalls = SSetInsert(state.visibleCalls, c),
          happensBefore = state.happensBefore.put(c, state.visibleCalls),
          invocationCalls = state.invocationCalls.put(state.currentInvocation,
            SSetInsert(SSetVar(state.invocationCalls.get(state.currentInvocation)), c))
        ).withTrace(s"call ${call.functionName.name}(${args.mkString(", ")})", source)

        follow(state2, ctxt)
      case InputAst.Assignment(source, varname, expr) =>
        debugPrint(s"Executing assignment in line ${source.getLine}")
        // use a new variable here to avoid duplication of expressions
        val v = ctxt.makeVariable(varname.name)(ctxt.translateSortVal(expr.getTyp))
        ctxt.addConstraint(s"${v.name}_assignment",
          v === ctxt.translateExprV(expr))
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(varname.name) -> v)
        ).withTrace(s"assignment $varname", source)
        follow(state2, ctxt)
      case InputAst.NewIdStmt(source, varname, typename) =>
        debugPrint(s"Executing new-id statement in line ${source.getLine}")
        val idType = typename.asInstanceOf[IdType]
        val vname = varname.name
        val newV: SVal[SortCustomUninterpreted] = ctxt.makeVariable(vname)(ctxt.translateSort(typename)).asInstanceOf[SVal[SortCustomUninterpreted]]
        ctxt.addConstraint(s"${vname}_new_id_fresh",
          state.generatedIds(idType)(newV) === SNone(SortInvocationId()))
        val state2 = state.copy(
          localState = state.localState + (ProgramVariable(vname) -> newV)
        ).withTrace(s"New-id $varname", source)
        follow(state2, ctxt)
      case InputAst.ReturnStmt(source, expr, assertions) =>
        debugPrint(s"Executing return statement in line ${source.getLine}")
        val returnv: SVal[SortValue] = ctxt.translateExprV(expr)

        val state2 = state.copy(
          invocationRes = state.invocationRes.put(state.currentInvocation, SReturnVal(ctxt.currentProcedure, returnv))
          // TODO update knownIds
        ).withTrace(s"Return ${returnv}", source)
        follow(state2, ctxt)
      case InputAst.AssertStmt(source, expr) =>
        debugPrint(s"Executing assert statement in line ${source.getLine}")
        ctxt.inContext(() => {
          ctxt.addConstraint("assert",
            SNot(ctxt.translateExpr(expr)))
          ctxt.check() match {
            case SymbolicContext.Unsatisfiable =>
            // check ok
            case SymbolicContext.Unknown =>
              throwSymbolicCounterExample(
                s"Assertion in line ${source.getLine} might not hold.",
                source.range,
                state,
                None,
                ctxt
              )
            case s: Satisfiable =>
              throwSymbolicCounterExample(
                s"Assertion in line ${source.getLine} failed: $expr",
                source.range,
                state,
                Some(s.model),
                ctxt
              )
          }
        })
        val state2 = state.withTrace(s"assert $expr", source)
        follow(state2, ctxt)
    }
  }

  def executeBeginAtomic(source: SourceTrace, state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.currentTransaction match {
      case Some(tx) =>
        throwSymbolicCounterExample(
          s"Already in a transaction",
          source.range,
          state,
          None,
          ctxt
        )
      case None =>
        // create variable for the new transaction
        val tx = ctxt.makeVariable[SortTxId]("tx")
        // transactionStatus S t = None;
        ctxt.addConstraint(s"${tx.name}_fresh",
          SEq(SMapGet(state.transactionStatus, tx), SNone(SortTransactionStatus())))
        // state_monotonicGrowth i S S'
        val state2 = monotonicGrowth(state, ctxt)
        // ⋀t. transactionOrigin S t ≜ i ⟷ transactionOrigin S' t ≜ i; ― ‹No new transactions are added to current invocId.›
        val t = ctxt.makeVariable[SortTxId]("t")
        ctxt.addConstraint("no_new_transactions_added_to_current",
          forall(t, (state.transactionOrigin.get(t) === SSome(state.currentInvocation))
            === (state2.transactionOrigin.get(t) === SSome(state.currentInvocation)))
        )
        // no new calls are added to current invocation:
        ctxt.addConstraint("no_new_calls_addded_to_current",
          SEq(
            state.invocationCalls.get(state.currentInvocation),
            state2.invocationCalls.get(state.currentInvocation)))
        // invariant_all S';
        ctxt.addConstraint("invariant_before_transaction",
          invariant(state2)(ctxt))
        // ⋀tx. transactionStatus S' tx ≠ Some Uncommitted;
        val tx2 = ctxt.makeVariable[SortTxId]("tx2")
        ctxt.addConstraint("no_uncommitted_transactions",
          forall(tx2, state2.transactionStatus.get(tx2) !== SSome(SUncommitted()))
        )
        // newTxns ⊆ dom (transactionStatus S');
        val newTxns = SSetVar[SortTxId](ctxt.makeVariable("newTxns"))
        ctxt.addConstraint("new_transactions_exist",
          newTxns.isSubsetOf(MapDomain(state2.transactionStatus))
        )


        // newCalls = callsInTransaction S' newTxns ↓ happensBefore S'
        val newCalls = SSetVar[SortCallId](ctxt.makeVariable("newCalls"))
        // TODO add restrictions to newCalls
        // vis' = vis ∪ newCalls
        val vis2 = SSetVar(ctxt.makeVariable[SortSet[SortCallId]]("vis"))
        ctxt.addConstraint("vis_update",
          SEq(vis2, SSetUnion(state2.visibleCalls, newCalls)))
        // TODO ⋀c. callOrigin S' c ≠ Some t


        state2.copy(
          transactionStatus = state2.transactionStatus.put(tx, SSome(SUncommitted())),
          transactionOrigin = state2.transactionOrigin.put(tx, SSome(state2.currentInvocation)),
          currentTransaction = Some(tx),
          visibleCalls = vis2
        )
    }
  }

  def monotonicGrowth(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    implicit val ictxt = ctxt
    // create new variables for new state
    val state2 = state.copy(
      calls = symbolicMapVar("calls"),
      happensBefore = symbolicMapVar("happensBefore"),
      callOrigin = symbolicMapVar("callOrigin"),
      transactionOrigin = symbolicMapVar("transactionOrigin"),
      transactionStatus = symbolicMapVar("transactionStatus"),
      invocationCalls = symbolicMapVar("invocationCalls"),
      generatedIds = generatedIdsVar,
      knownIds = knownIdsVar
    )

    // TODO add constraints between old and new state

    state2
  }

  def executeEndAtomic(state: SymbolicState, ctxt: SymbolicContext): SymbolicState = {
    state.copy(
      currentTransaction = None,
      transactionStatus = state.transactionStatus.put(state.currentTransaction.get, SSome(SCommitted()))
    )
  }


  def printModel(ctxt: SymbolicContext)(model: Model, state: SymbolicState): Doc = {
    import crdtver.utils.PrettyPrintDoc._

    implicit def exprToDoc(e: SVal[_]): Doc = e.toString

    "calls = " <> model.evaluate(state.calls) </>
      "happensBefore = " <> model.evaluate(state.happensBefore) </>
      "callOrigin = " <> model.evaluate(state.callOrigin) </>
      "transactionOrigin = " <> model.evaluate(state.transactionOrigin) </>
      "transactionStatus = " <> model.evaluate(state.transactionStatus) </>
      //      "generatedIds = " <> model.evaluate(state.generatedIds) </>
      //      "knownIds = " <> model.evaluate(state.knownIds) </>
      "invocationCalls = " <> model.evaluate(state.invocationCalls) </>
      "invocationOp = " <> model.evaluate(state.invocationOp) </>
      "invocationRes = " <> model.evaluate(state.invocationRes) </>
      "currentInvocation = " <> model.evaluate(state.currentInvocation) </>
      "currentTransaction = " <> state.currentTransaction.map(v => model.evaluate(v)).toString </>
      "localState = " <> sep(line, state.localState.toList.map { case (k, v) => k.name <+> ":=" <+> model.evaluate(v) }) </>
      "visibleCalls = " <> model.evaluate(state.visibleCalls) </>
      "currentCallIds = " <> sep(", ", state.currentCallIds.map("" <> model.evaluate(_))) </>
      "satisfiable = " <> state.satisfiable.toString </>
      model.toString
  }


  def throwSymbolicCounterExample(message: String, source: SourceRange, state: SymbolicState, model: Option[Model], ctxt: SymbolicContext): Nothing = {
    throw new SymbolicExecutionException(makeSymbolicCounterExample(message, source, state, model, ctxt))
  }

  def makeSymbolicCounterExample(message: String, source: SourceRange, state: SymbolicState, model: Option[Model],
    ctxt: SymbolicContext
  ): SymbolicCounterExample = {

    val name = s"${ctxt.currentProcedure}_line${source.start.line}"

    val isabelleTranslation: String = createIsabelleDefs(name, ctxt.datypeImpl, ctxt.allConstraints())
    val smtTranslation = ctxt.z3Translation.exportConstraints(ctxt.allConstraintsSimplified())


    SymbolicCounterExample(
      message = message,
      errorLocation = source,
      trace = state.trace,
      isabelleTranslation = isabelleTranslation,
      smtTranslation = smtTranslation,
      model = model.map(extractModel(state, _))
    )
  }

  private def extractModel(state: SymbolicState, model: Model): SymbolicCounterExampleModel = {

    val iState: Interpreter.State = extractInterpreterState(state, model)

    debugPrint(s"STATE =\n$iState")

    val (dot, svg) = Visualization.renderStateGraph(prog, iState)
    SymbolicCounterExampleModel(
      iState,
      svg,
      dot
    )
  }

  def intersection[T](list: List[Set[T]]): Set[T] = list match {
    case List() => Set()
    case List(s) => s
    case x :: xs =>
      x.intersect(intersection(xs))
  }

  private def extractInterpreterState(state: SymbolicState, model: Model): Interpreter.State = {


    debugPrint(s"callsS = ${model.evaluate(state.calls)}")

    val translateCallId: StringBasedIdGenerator[SVal[SortCallId], CallId] =
      new StringBasedIdGenerator(CallId(_))

    val translateTransactionId: StringBasedIdGenerator[SVal[SortTxId], TransactionId] =
      new StringBasedIdGenerator(TransactionId(_))

    val translateInvocationId: StringBasedIdGenerator[SVal[SortInvocationId], InvocationId] =
      new StringBasedIdGenerator(InvocationId(_))


    def extractMap[K <: SymbolicSort, V <: SymbolicSort](cs: SVal[SortMap[K, V]]): Map[SVal[K], SVal[V]] = cs match {
      case SymbolicMapUpdated(k, v, b) =>
        extractMap(b) + (k.cast[K] -> v.cast[V])
      case SymbolicMapEmpty(dv) =>
        debugPrint(s"Empty with default $dv")
        Map()
      case x =>
        throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
    }

    def extractSet[T <: SymbolicSort](s: SVal[SortSet[T]]): Set[SVal[T]] = s match {
      case SSetInsert(base, v) =>
        extractSet(base) + v
      case SSetEmpty() =>
        Set()
      case SSetUnion(a, b) =>
        extractSet(a) ++ extractSet(b)
      case x =>
        throw new RuntimeException(s"unhandled case ${x.getClass}:\n$x")
    }

    def getSnapshotTime(c: SVal[SortCallId]): SnapshotTime = {
      val hb = model.evaluate(state.happensBefore.get(c))
      SnapshotTime(extractSet(hb).map(translateCallId))
    }

    def getCallTransaction(c: SVal[SortCallId]): TransactionId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          TransactionId(-1)
        case SSome(t) =>
          translateTransactionId(t)
        case _ => ???
      }
    }

    def getTransactionOrigin(tx: SVal[SortTxId]): InvocationId = {
      val io: SVal[SortOption[SortInvocationId]] = model.evaluate(state.transactionOrigin.get(tx))
      io match {
        case SNone(ty) =>
          debugPrint(s"!!! Transaction $tx which is not in an invocation")
          InvocationId(-2)
        case SSome(i) =>
          translateInvocationId(i)
        case _ => ???
      }
    }

    def getCallInvocation(c: SVal[SortCallId]): InvocationId = {
      val to: SVal[SortOption[SortTxId]] = model.evaluate(state.callOrigin.get(c))
      to match {
        case SNone(ty) =>
          debugPrint(s"!!! Call $c is not in a transaction")
          InvocationId(-1)
        case SSome(tx) =>
          getTransactionOrigin(tx)
        case _ => ???
      }
    }

    def translateCall(value: SVal[SortCall]): DataTypeValue = value match {
      case SCallInfo(o, args) =>
        DataTypeValue(o, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationOp(value: SVal[SortInvocationInfo]): DataTypeValue = value match {
      case SInvocationInfo(procName, args) =>
        DataTypeValue(procName, args.map(AnyValue(_)))
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }

    def translateInvocationRes(value: SVal[SortInvocationRes]): Option[DataTypeValue] = value match {
      case SReturnVal(m, v) =>
        Some(DataTypeValue(m, List(AnyValue(v))))
      case SReturnValNone() =>
        None
      case x => throw new RuntimeException(s"Unhandled case ${x.getClass}: $x")
    }


    val scalls: Map[SVal[SortCallId], SVal[SortCall]] = extractMap(model.evaluate(state.calls))
    val sInvocationOp: Map[SVal[SortInvocationId], SVal[SortInvocationInfo]] = extractMap(model.evaluate(state.invocationOp))
    val sInvocationRes: Map[SVal[SortInvocationId], SVal[SortInvocationRes]] = extractMap(model.evaluate(state.invocationRes))

    // TODO replace this with tree traversal collecting all occurrences
    for (c <- scalls.keys) {
      translateCallId(c)
    }
    for (i <- sInvocationOp.keys) {
      translateInvocationId(i)
    }
    for (i <- sInvocationRes.keys) {
      translateInvocationId(i)
    }


    val calls: Map[CallId, CallInfo] =
      (for ((sc, c) <- translateCallId) yield {
        c -> CallInfo(
          c,
          translateCall(scalls(sc)),
          getSnapshotTime(sc),
          getCallTransaction(sc),
          getCallInvocation(sc),
        )
      }).toMap

    debugPrint(s"calls = $calls")


    val transactions: Map[TransactionId, TransactionInfo] =
      (for ((st, t) <- translateTransactionId) yield {
        val callsInT: List[CallInfo] = calls.values.filter(_.callTransaction == t).toList
        t -> TransactionInfo(
          t,
          SnapshotTime(intersection(callsInT
            .map(_.callClock.snapshot))),
          getTransactionOrigin(st),
          callsInT,
          true
        )
      }).toMap

    debugPrint(s"transactions = $transactions")

    val invocations: Map[InvocationId, InvocationInfo] =
      (for ((si, i) <- translateInvocationId) yield {
        val invocationOp: SVal[SortInvocationInfo] = model.evaluate(state.invocationOp.get(si))
        val invocationRes: SVal[SortInvocationRes] = model.evaluate(state.invocationRes.get(si))
        i -> InvocationInfo(
          i,
          translateInvocationOp(invocationOp),
          translateInvocationRes(invocationRes)
        )
      }).toMap

    debugPrint(s"invocations = $invocations")

    Interpreter.State(
      calls = calls,
      transactions = transactions,
      invocations = invocations
    )
  }

  /** Checks the invariant in the given state.
    * Returns None if no problem was found and an error message otherwise.
    *
    * Implementation details:
    * The check is done by adding the negated invariant to the current context and checking if
    * the resulting context is unsatisfiable.
    *
    * If the context is satisfiable this means that there is a model such that all assumptions
    * in the current context are true, but the invariant is false.
    *
    * */
  private def checkInvariant(source: SourceTrace, ctxt: SymbolicContext, state: SymbolicState, where: String): Option[SymbolicCounterExample] = {

    debugPrint("checkInvariant: before")
    ctxt.check() match {
      case Unsatisfiable =>
        // ok
        throw new RuntimeException("failed before invariant-check ...")
      case Unknown =>
        debugPrint("checkInvariant1: unknown ... ")
      //        throw new RuntimeException(s"")
      case s: Satisfiable =>
        debugPrint("checkInvariant1: Satisfiable ")
    }

    ctxt.inContext(() => {
      ctxt.addConstraint("invariant_not_violated",
        SNot(invariant(state)(ctxt)))
      createIsabelleDefs(s"${
        ctxt.currentProcedure
      }_$where", ctxt.datypeImpl, ctxt.allConstraints())
      ctxt.check() match {
        case Unsatisfiable =>
          debugPrint("checkInvariant: unsat, ok")
          // ok
          None
        case Unknown =>
          debugPrint("checkInvariant: unknown")
          Some(makeSymbolicCounterExample(
            s"Could not prove invariant $where",
            source.range,
            state,
            None,
            ctxt
          ))
        case s: Satisfiable =>
          debugPrint("checkInvariant: satisfiable, computing model ...")
          val model = s.model
          val str = printModel(ctxt)(model, state).prettyStr(200)
          debugPrint(str)
          Some(makeSymbolicCounterExample(
            s"Invariant does not hold $where",
            source.range,
            state,
            Some(model),
            ctxt
          ))
      }
    })
  }

  private def invariant(state: SymbolicState)(implicit ctxt: SymbolicContext): SVal[SortBoolean] = {
    val invExprs: List[SVal[SortBoolean]] = for (inv <- prog.invariants) yield {
      ExprTranslation.translate(inv.expr)(SymbolicSort.bool, ctxt, state)
    }
    SVal.and(invExprs)
  }


  private def makeVariablesForParameters(ctxt: SymbolicContext, params: List[InputAst.InVariable]): List[(ProgramVariable, SVal[SortValue])] = {
    for (p <- params) yield
      ProgramVariable(p.name.name) -> ctxt.makeVariable(p.name + "_init")(ctxt.translateSortVal(p.typ))
  }


}

case class SymbolicCounterExample(
  message: String,
  errorLocation: SourceRange,
  trace: Trace,
  isabelleTranslation: String,
  smtTranslation: String,
  model: Option[SymbolicCounterExampleModel]
)

case class SymbolicCounterExampleModel(
  state: Interpreter.State,
  counterExampleSvg: String,
  counterExampleDot: String
) {
  override def toString: String = s"SymbolicCounterExampleModel($state)"
}