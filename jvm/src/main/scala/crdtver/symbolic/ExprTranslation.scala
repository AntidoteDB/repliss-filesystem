package crdtver.symbolic

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.Identifier
import crdtver.language.TypedAst.FunctionKind.{FunctionKindCrdtQuery, FunctionKindDatatypeConstructor}
import crdtver.language.{InputAst, TypedAst}
import crdtver.language.TypedAst._
import crdtver.symbolic
import crdtver.utils.Helper
import crdtver.symbolic.SVal.MapGetExtension
import crdtver.symbolic.SVal.SetSValExtensions

object ExprTranslation {

  def translateType(typ: TypedAst.InTypeExpr)(implicit ctxt: SymbolicContext): SymbolicSort =
    try {
      typ match {
        case CallIdType() => SortCallId()
        case CallInfoType() => SortCall()
        case BoolType() => SortBoolean()
        case IntType() => SortInt()
        case InvocationResultType() =>
          SortInvocationRes()
        case FunctionType(argTypes, returnType, kind) => ???
        case TransactionIdType() => SortTxId()
        case InvocationInfoType() => SortInvocationInfo()
        case AnyType() => ???
        case st: IdType =>
          SortCustomUninterpreted(st.name)
        case st: SimpleType =>
          ctxt.getCustomType(st)
        case SomeOperationType() => SortCall()
        case OperationType(name) => SortCall()
        case TypedAst.InvocationIdType() => SortInvocationId()
        case TypedAst.TypeVarUse(name) =>
          throw new Exception(s"Cannot translate TypeVarUse $name")
        case UnitType() =>
          throw new Exception(s"Cannot translate Unit type")
      }
    } catch {
      case err: Exception =>
        throw new Exception(s"Error translating type $typ", err)
    }

  def translateType(st: TypedAst.IdType): SortCustomUninterpreted =
    SortCustomUninterpreted(st.name)

  /** determines the invocation of a call */
  def callInvocation(cId: SVal[SortCallId])(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SortOption[SortInvocationId]] = {
    val tx = ctxt.makeBoundVariable[SortTxId]("matched_tx")
    SOptionMatch(
      state.callOrigin.get(cId),
      tx,
      state.transactionOrigin.get(tx),
      SNone(SortInvocationId())
    )
  }

  def translateBuiltin(expr: ApplyBuiltin)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[_ <: SymbolicSort] = {
    val args: List[SVal[_]] = expr.args.map(translateUntyped)
    expr.function match {
      case BF_upperBoundedBy() => // TODOO: hard code
       val left: SVal[SymbolicSort] = castSymbolicSort(args(0))
       val right: SVal[SymbolicSort] = castSymbolicSort(args(1))
       // automatically adapt to option types
       // TODO maybe adapt to option types in the frontend
       if (left.typ == SortOption(right.typ))
         SEq(castSymbolicSort(left), castSymbolicSort(SSome(right)))
       else if (SortOption(left.typ) == right.typ)
         SEq(castSymbolicSort(SSome(left)), castSymbolicSort(right))
       else
         SEq(left, right)
      case BF_isVisible() =>
        state.visibleCalls.contains(cast(args(0)))
      case BF_happensBefore(on) =>
        on match {
          case HappensBeforeOn.Unknown() =>
            ???
          case HappensBeforeOn.Call() =>
            val c1 = cast[SortCallId](args(0))
            val c2 = cast[SortCallId](args(1))
            callHappensBefore(c1, c2)
          case HappensBeforeOn.Invoc() =>
            val i1: SVal[SortInvocationId] = cast(args(0))
            val i2: SVal[SortInvocationId] = cast(args(1))

            invocationHappensBefore(i1, i2)(ctxt, state)
          //            // invocation
          //            val c1 = ctxt.makeVariable[SortCallId]("c1")
          //            val c2 = ctxt.makeVariable[SortCallId]("c1")
          //            // exists c1, c2 :: c1.origin == i1 && c2.origin == i2 && c1 happened before c2
          //            val existsHb =
          //              QuantifierExpr(QExists(), c1,
          //                QuantifierExpr(QExists(), c2,
          //                  SAnd(
          //                    SAnd(
          //                      SEq(callInvocation(c1), SSome(i1)),
          //                      SEq(callInvocation(c2), SSome(i2))),
          //                    callHappensBefore(c1, c2))))
          //            // forall c1, c2 :: (c1.origin == i1 && c2.origin == i2) ==> c1 happened before c2
          //            val allHb =
          //              QuantifierExpr(QForall(), c1,
          //                QuantifierExpr(QForall(), c2,
          //                  SImplies(
          //                    SAnd(
          //                      SEq(callInvocation(c1), SSome(i1)),
          //                      SEq(callInvocation(c2), SSome(i2))),
          //                    callHappensBefore(c1, c2))))
          //            SAnd(existsHb, allHb)
        }
      case BF_sameTransaction() =>
        SEq(state.callOrigin.get(cast(args(0))), state.callOrigin.get(cast(args(1))))
      case BF_less() =>
        SLessThan(cast(args(0)), cast(args(1)))
      case BF_lessEq() =>
        SLessThanOrEqual(cast(args(0)), cast(args(1)))
      case BF_greater() =>
        SLessThan(cast(args(1)), cast(args(0)))
      case BF_greaterEq() =>
        SLessThanOrEqual(cast(args(1)), cast(args(0)))
      case BF_equals() =>
        val left: SVal[SymbolicSort] = castSymbolicSort(args(0))
        val right: SVal[SymbolicSort] = castSymbolicSort(args(1))
        // automatically adapt to option types
        // TODO maybe adapt to option types in the frontend
        if (left.typ == SortOption(right.typ))
          SEq(castSymbolicSort(left), castSymbolicSort(SSome(right)))
        else if (SortOption(left.typ) == right.typ)
          SEq(castSymbolicSort(SSome(left)), castSymbolicSort(right))
        else
          SEq(left, right)
      case BF_notEquals() =>
        SNotEq(castSymbolicSort(args(0)), castSymbolicSort(args(1)))
      case BF_and() =>
        SAnd(cast(args(0)), cast(args(1)))
      case BF_or() =>
        SOr(cast(args(0)), cast(args(1)))
      case BF_implies() =>
        SImplies(cast(args(0)), cast(args(1)))
      case BF_iff() =>
        SEq[SortBoolean](cast(args(0)), cast(args(1)))
      case BF_not() =>
        SNot(cast(args(0)))
      case BF_plus() =>
        SBinaryInt(SPlus(), cast(args(0)), cast(args(1)))
      case BF_minus() =>
        SBinaryInt(SMinus(), cast(args(0)), cast(args(1)))
      case BF_mult() =>
        SBinaryInt(SMult(), cast(args(0)), cast(args(1)))
      case BF_div() =>
        SBinaryInt(SDiv(), cast(args(0)), cast(args(1)))
      case BF_mod() =>
        SBinaryInt(SMod(), cast(args(0)), cast(args(1)))
      case BF_getOperation() =>
        state.calls.get(cast(args(0)))
      case BF_getInfo() =>
        state.invocationOp.get(cast(args(0)))
      case BF_getResult() =>
        state.invocationRes.get(cast(args(0)))
      case BF_getOrigin() =>
        expr.args(0).getTyp match {
          case CallIdType() =>
            val callId = cast[SortCallId](args(0))
            val tx = ctxt.makeBoundVariable("tx")(SortTxId())
            SOptionMatch(
              state.callOrigin.get(callId),
              tx,
              state.transactionOrigin.get(tx),
              SNone(SortInvocationId())
            )
          case TransactionIdType() =>
            val tx = cast[SortTxId](args(0))
            state.transactionOrigin.get(tx)
          case x => Helper.unexpected(x)
        }
      case BF_getTransaction() =>
        state.callOrigin.get(cast(args(0)))
      case BF_inCurrentInvoc() =>
        //        SEq(state.currentInvocation, state.transactionOrigin.get(state.callOrigin.get(cast(args(0)))))
        ???
      case BF_distinct() =>
        SDistinct(args.asInstanceOf[List[SVal[SymbolicSort]]])
    }
  }

  def invocationHappensBefore(i1: SVal[SortInvocationId], i2: SVal[SortInvocationId])(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SortBoolean] = {
    val ca = ctxt.makeBoundVariable[SortCallId]("ca")
    val cb = ctxt.makeBoundVariable[SortCallId]("cb")

    SAnd(
      SAnd(
        SNotEq(state.invocationCalls.get(i1), SSetEmpty(SortCallId())),
        SNotEq(state.invocationCalls.get(i2), SSetEmpty(SortCallId()))
      ),
      symbolic.QuantifierExpr(QForall(), ca,
        SImplies(
          SSetContains(state.invocationCalls.get(i1), ca)
          , symbolic.QuantifierExpr(QForall(), cb,
            SImplies(
              SSetContains(state.invocationCalls.get(i2), cb),
              callHappensBefore(ca, cb)))))
    )
  }

  /** checks that c1 happened before c2 */
  def callHappensBefore(c1: SVal[SortCallId], c2: SVal[SortCallId])(implicit state: OperationContext): SVal[SortBoolean] = {
    SSetContains[SortCallId](state.happensBefore.get(c2), c1)
  }

  def translate[T <: SymbolicSort](expr: InExpr)(implicit sort: T, ctxt: SymbolicContext, state: SymbolicState): SVal[T] = {
    val res: SVal[_] = translateUntyped(expr)
    cast(res)
  }


  private def debugPrint(str: String): Unit = {}

  def translateUntyped(expr: InExpr)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SymbolicSort] = {
    try {
      expr match {
        case TypedAst.VarUse(source, typ, name) =>
          state.lookupLocal(name).upcast
        case TypedAst.BoolConst(source, typ, value) =>
          SBool(value).upcast
        case TypedAst.IntConst(source, typ, value) =>
          ConcreteVal(value)(SortInt())
        case expr: TypedAst.CallExpr => expr match {
          case fc@TypedAst.FunctionCall(source, typ, functionName, _, args, kind) =>
            kind match {
              case FunctionKind.FunctionKindDatatypeConstructor() =>
                val translatedArgs = args.map(translateUntyped(_))
                val t = translateType(expr.getTyp).asInstanceOf[SortDatatype]
                SDatatypeValue(ctxt.datypeImpl(ctxt.translateSortDatatype(typ)), functionName.name, translatedArgs, t).upcast
              case FunctionKind.FunctionKindCrdtQuery() =>
                translateCrdtQuery(fc)
            }
          case bi: ApplyBuiltin =>
            translateBuiltin(bi).upcast
        }
        case q: TypedAst.CrdtQuery =>
          val qryOp = q.qryOp
          val flat = ctxt.prog.programCrdt.toFlatQuery[InExpr](qryOp)
            .getOrElse(throw new Exception(s"Could not get flat query for $qryOp"))
          println(s"Flat $q --> $flat")
          val qryName = flat.name
          val src = q.source
          val fc = FunctionCall(src, q.typ, Identifier(src, qryName), List(), flat.args, FunctionKindCrdtQuery())
          translateUntyped(fc)
        case TypedAst.QuantifierExpr(source, quantifier, vars, e) =>

          val q = quantifier match {
            case InputAst.Forall() => QForall()
            case InputAst.Exists() => QExists()
          }

          def tr(vars: List[InVariable], state: SymbolicState): SVal[SortBoolean] =
            vars match {
              case Nil =>
                translate(e)(implicitly, implicitly, state)
              case v :: vs =>
                val vt = ctxt.makeBoundVariable(v.name.name)(translateType(v.typ))
                val state2 = state.withLocal(ProgramVariable(v.name.name), vt)
                symbolic.QuantifierExpr(q, vt, tr(vs, state2))
            }

          tr(vars, state).upcast

        case expr: AggregateExpr =>
          val vars = expr.vars.map(v => ctxt.makeBoundVariable(v.name.name)(translateType(v.typ)))

          val agg = SAggregateSum()

          var state2 = state
          for ((v, vt) <- expr.vars.zip(vars))
            state2 = state2.withLocal(ProgramVariable(v.name.name), vt)

          SAggregateExpr[SortInt](
            agg,
            vars,
            translate(expr.filter)(SortBoolean(), ctxt, state2),
            translate(expr.elem)(agg.typ, ctxt, state2)
          ).upcast
        case InAllValidSnapshots(_, e) =>
          // for the verification conditions, we not actually check/assume this in all possible valid snapshots,
          // because the theorem provers cannot handle the resulting complex formula.
          // Instead we give two specific snapshots with the arbitrary but fixed set snapshotAdditions.
          // This weakens the assumptions, but not the proof obligation.
          // Still it simplifies the burden of the theorem provers, since the universal quantifier is already instantiated.

          val e1: SVal[SortBoolean] = translate(e)(implicitly, implicitly, state.copy(visibleCalls = state.snapshotAddition))
          val e2: SVal[SortBoolean] = translate(e)(implicitly, implicitly, state.copy(visibleCalls = state.visibleCalls.union(state.snapshotAddition)))

          (SNamedVal("for_snapshot_additions", e1) &&
            SNamedVal("for_snapshot_additions_with_visibleCalls", e2)).upcast
      }
    } catch {
      case e: Throwable =>
        throw new RuntimeException(s"Error when translating ${expr.printAst}", e)
    }
  }

  private def translateCrdtQuery(q: FunctionCall)(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SymbolicSort] = {
    require(q.kind == FunctionKindCrdtQuery())
    val queryName = q.functionName.name
    ctxt.findQuery(queryName) match {
      case None =>
        throw new RuntimeException(s"Could not find function $queryName\n${ctxt.prog.programCrdt.queryDefinitions().map(_.name.name).mkString(", ")}")
      case Some(query) =>
        // bind the parameter values:
        var state2 = state
        val translatedArgs = q.args.map(translateUntyped(_))

        for ((p, a) <- query.params.zip(translatedArgs)) {
          state2 = state2.withLocal(ProgramVariable(p.name.name), a)
        }

        query.implementation match {
          case Some(impl) =>
            // inline the implementation:
            SNamedVal(s"${query.name}_res", translateUntyped(impl)(ctxt, state2))
          case None =>
            // create a new symbolic variable for the result_
            val result = ctxt.makeVariable(query.name.name)(translateType(query.returnType))
            query.ensures match {
              case Some(postCondition) =>
                // assume the postcondition:
                state2 = state2.withLocal(ProgramVariable("result"), result)
                SChooseSome(
                  SNamedVal(s"query_${query.name}_postcondition",
                    translate(postCondition)(SortBoolean(), ctxt, state2)),
                  result
                )(result.typ)
              case None =>
                println(s"Warning: Query $queryName does not have a specification.")
                result
            }
        }
    }
  }

  def cast[T <: SymbolicSort](e: SVal[_])(implicit sort: T, state: SymbolicState): SVal[T] = {
    if (e.typ != sort) {
      throw new RuntimeException(s"Expected expression of type $sort, but got $e of type ${e.typ}")
    }
    e.asInstanceOf[SVal[T]]
  }

  def castList[T <: SymbolicSort](es: List[SVal[_]])(implicit sort: T, state: SymbolicState): List[SVal[T]] =
    es.map(cast(_)(sort, state))

  def castSymbolicSort(e: SVal[_]): SVal[SymbolicSort] = {
    e.asInstanceOf[SVal[SymbolicSort]]
  }


}
