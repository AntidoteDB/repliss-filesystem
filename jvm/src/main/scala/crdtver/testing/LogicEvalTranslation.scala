package crdtver.testing

import com.github.peterzeller.logiceval.SimpleLogic
import com.github.peterzeller.logiceval.SimpleLogic.{BoolType, Expr, Opaque, _}
import crdtver.language.InputAst.BuiltInFunc
import crdtver.language.InputAst.BuiltInFunc.HappensBeforeOn
import crdtver.language.{InputAst, TypedAst}
import crdtver.language.TypedAst._
import crdtver.testing.Interpreter._
import crdtver.utils.Helper.unexpected
import crdtver.utils.MapUtils.MapExtensions
import crdtver.utils.{MathUtils, myMemo}

object LogicEvalTranslation {

  case class InterpreterEnv(
    state: State,
    localState: LocalState,
    interpreter: Interpreter
  ) extends Env {

    override def customTypeValues[T](c: CustomType[T]): Iterable[T] = {

      val res: Iterable[_] =
        c match {
          case T_Int => (0 to interpreter.domainSize)
          case T_Unit => Set(())
          case T_CallId => state.calls.keys
          case T_InvocationId => state.invocations.keys
          case T_TransactionId => state.transactions.keys
          case CustomType(s"#IdType$name") =>
            state.generatedIds.getOrElse(IdType(name)(), Set()).map(_.value)
          case _ => interpreter.customTypeDomain(c.name).map(_.value)
        }
      res.asInstanceOf[Iterable[T]]
    }
  }

  case class Ctxt(
    prog: InProgram,
    vars: Map[String, Var[_]] = Map()
  )

  val T_Any: Type[Any] = CustomType[Any]("Any")(_.isInstanceOf[Any])
  implicit val T_Bool: BoolType = SimpleLogic.BoolType()
  implicit val T_Int: CustomType[BigInt] = CustomType[BigInt]("BigInt")(_.isInstanceOf[BigInt])
  implicit val T_Unit: CustomType[Unit] = CustomType[Unit]("Unit")(_.isInstanceOf[Unit])
  implicit val T_CallId: CustomType[CallId] = CustomType[CallId]("CallId")(_.isInstanceOf[CallId])
  //  implicit val T_CallInfo: CustomType[CallInfo] = CustomType[CallInfo]("CallInfo")(_.isInstanceOf[CallInfo])
  implicit val T_InvocationId: CustomType[InvocationId] = CustomType[InvocationId]("InvocationIdType")(_.isInstanceOf[InvocationId])
  implicit val T_TransactionId: CustomType[TransactionId] = CustomType[TransactionId]("TransactionIdType")(_.isInstanceOf[TransactionId])
  //  implicit val T_InvocationInfo: CustomType[InvocationInfo] = CustomType[InvocationInfo]("InvocationInfoType")(_.isInstanceOf[InvocationInfo])
  //  implicit val InvocationResultType: CustomType[InvocationResult] = CustomType[InvocationResult]("InvocationResultType")
  //  implicit val SomeOperationType: CustomType[SomeOperationType] = CustomType[SomeOperationType]("SomeOperationType")
  //  implicit val OperationType: CustomType[OperationType] = CustomType[OperationType]("OperationType")
  implicit val T_DataTypeValue: CustomType[DataTypeValue] = CustomType[DataTypeValue]("DataTypeValue")(_.isInstanceOf[DataTypeValue])

  implicit def T_Set[T](implicit e: Type[T]): SetType[T] = SetType(e)

  implicit def T_Map[K, V](implicit k: Type[K], v: Type[V]): MapType[K, V] = MapType(k, v)

  implicit def T_Pair[A, B](implicit a: Type[A], b: Type[B]): PairType[A, B] = PairType(a, b)

  def translateDt[T](typ: InTypeExpr)(implicit ctxt: Ctxt): Datatype[T] = {
    val prog = ctxt.prog
    typ match {
      case AnyType() => ???
      case UnitType() => ???
      case TypedAst.BoolType() => ???
      case IntType() => ???
      case CallIdType() => ???
      case CallInfoType() =>
        Datatype[DataTypeValue]("CallInfo", List(
          dtCase("Op", List(translateType(ctxt.prog.programCrdt.operationType))),
          dtCase("Qry", List(translateType(ctxt.prog.programCrdt.queryType))),
          dtCase("NoCall", List()),
        )).asInstanceOf[Datatype[T]]
      case InvocationIdType() => ???
      case TransactionIdType() => ???
      case InvocationInfoType() =>
        val procCases =
          for (proc <- prog.procedures) yield {
            val argTypes = proc.params.map(p => translateType(p.typ))
            val pName = proc.name.name
            dtCase(pName, argTypes)
          }

        Datatype[DataTypeValue]("InvocationInfo",
          dtCase("NoInfo", List()) :: procCases)
          .asInstanceOf[Datatype[T]]
      case InvocationResultType() =>
        val procCases =
                  for (proc <- prog.procedures) yield {
                    val argTypes = proc.returnType match {
                      case UnitType() => List()
                      case other => List(translateType(other))
                    }
                    val pName = s"${proc.name.name}_res"
                    dtCase(pName, argTypes)
                  }
        Datatype[DataTypeValue]("InvocationResult", dtCase("NoResult", List()) :: procCases)
          .asInstanceOf[Datatype[T]]
      case SomeOperationType() => ???
      case OperationType(name) => ???
      case FunctionType(argTypes, returnType, functionKind) => ???
      case SimpleType(name, typeArgs) =>
        val dt: InTypeDecl = prog.findDatatype(name).get

        val cases: List[DtCase[T]] =
          for (c <- dt.dataTypeCases) yield {
            val cName = c.name.name

            def construct(args: List[Any]): DataTypeValue =
              DataTypeValue(cName, args.map(AnyValue))

            val argTypes =
              for (a <- c.params) yield translateType(a.typ)

            DtCase[DataTypeValue](cName, argTypes)(construct, x => x.operationName == cName, _.args.map(_.value))
              .asInstanceOf[DtCase[T]]
          }

        Datatype(dt.name.name, cases)
      case TypeVarUse(name) => ???
      case IdType(name) => ???
    }

  }

  private def dtCase[T](pName: String, argTypes: List[Type[_]]): DtCase[DataTypeValue] = {
    DtCase[DataTypeValue](pName, argTypes)(args => DataTypeValue(pName, args.map(AnyValue)), _.operationName == pName, _.args.map(_.value))
  }

  def translateExpr[T](expr: InExpr)(implicit ctxt: Ctxt): Expr[T] = {
    translateExprI(expr).asInstanceOf[Expr[T]]
  }

  def translateType[T](typ: InTypeExpr)(implicit ctxt: Ctxt): Type[T] =
    translateTypeI(typ).asInstanceOf[Type[T]]


  private def translateTypeI(typ: InTypeExpr)(implicit ctxt: Ctxt): Type[_] =
    typ match {
      case AnyType() => ???
      case UnitType() => T_Unit
      case TypedAst.BoolType() => BoolType()
      case IntType() => T_Int
      case CallIdType() => T_CallId
      case CallInfoType() => translateDt(typ)
      case InvocationIdType() => T_InvocationId
      case TransactionIdType() => T_TransactionId
      case InvocationInfoType() => translateDt(typ)
      case InvocationResultType() => T_DataTypeValue
      case SomeOperationType() => T_DataTypeValue
      case OperationType(name) => T_DataTypeValue
      case FunctionType(argTypes, returnType, functionKind) => ???
      case SimpleType(name, typeArgs) =>
        val t = ctxt.prog.findType(name).get
        if (t.dataTypeCases.nonEmpty) {
          val cases =
            for (c <- t.dataTypeCases) yield {
              dtCase(c.name.name, c.params.map(p => translateType(p.typ)))
            }
          Datatype(name, cases)
        } else {
          // other types are represented by Strings
          CustomType(name)(_.isInstanceOf[String])
        }
      case TypeVarUse(name) => ???
      case IdType(name) => CustomType(s"#IdType$name")(_.isInstanceOf[String])
    }

  def tupleType[T](argTypes: List[Type[_]]): Type[T] = (argTypes match {
    case List() => T_Unit
    case List(t) => t
    case List(x,y) => PairType(x, y)
    case x::xs =>
      PairType(x, tupleType(xs))
  }).asInstanceOf[Type[T]]

  def tupleExpr[T](exprs: List[Expr[_]]): Expr[T] = (exprs match {
    case List() => ConstExpr(())(T_Unit)
    case List(t) => t
    case List(x,y) => Pair(x, y)
    case x::xs =>
      Pair(x, tupleExpr(xs))
  }).asInstanceOf[Expr[T]]

  def unfoldExpr(tuple: Any, count: Int): List[Any] = {
    if (count == 0) List()
    else if (count == 1) List(tuple)
    else tuple match {
      case (x, y) =>
        x :: unfoldExpr(y, count - 1)
    }
  }


  def translateExprI(expr: InExpr)(implicit ctxt: Ctxt): Expr[_] =
    expr match {
      case TypedAst.VarUse(source, typ, name) =>
        ctxt.vars.getE(name)
      case TypedAst.BoolConst(source, typ, value) =>
        bool(value)
      case TypedAst.IntConst(source, typ, value) =>
        ConstExpr(value)(T_Int)
      case expr: TypedAst.CallExpr =>
        expr match {
          case TypedAst.FunctionCall(source, typ, functionName, typeArgs, args, kind) =>
            kind match {
              case FunctionKind.FunctionKindDatatypeConstructor() =>
                val name = functionName.name

                val dt = translateDt[Any](typ)
                val constr = dt.cases.find(_.name == name).get

                ConstructDt(dt, constr, args.map(translateExpr))
              case FunctionKind.FunctionKindCrdtQuery() =>
                // TODO evaluate query
                val count = args.size
                def f(e: Env, a: Any): Any = {
                  val args = unfoldExpr(a, count).map(AnyValue)
                  val env = e.asInstanceOf[InterpreterEnv]
                  val r = env.interpreter.evaluateQuery(env.localState, env.state, functionName, args)(env.interpreter.defaultAnyValueCreator)
                  r.value
                }
                val argsTr = args.map(translateExpr)
                val argTypes = args.map(a => translateType(a.getTyp))
                val argTypeTup: Type[Any] = tupleType(argTypes)
                Opaque[Any, Any](argTypeTup, translateType(typ), f, tupleExpr(argsTr))
            }
          case TypedAst.ApplyBuiltin(source, typ, function, args) =>
            def argsT[T](i: Int): Expr[T] = translateExpr(args(i))

            function match {
              case BuiltInFunc.BF_isVisible() =>
                IsElem(argsT(0), env((e: InterpreterEnv) => e.localState.visibleCalls))
              case BuiltInFunc.BF_upperBoundedBy() => // TODOO: hard code
                Eq(argsT(0), argsT(1))
                // op[(Expr[Any], Expr[Any]), Boolean](e => true, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_happensBefore(on) =>
                on match {
                  case HappensBeforeOn.Unknown() => unexpected(on)
                  case HappensBeforeOn.Call() =>
                    IsElem(argsT(0), env[CallId, Set[CallId]]((e, a) => e.state.calls.get(a).map(_.callClock.snapshot).getOrElse(Set()), argsT(1)))
                  case HappensBeforeOn.Invoc() =>
                    env[(InvocationId, InvocationId), Boolean]((e, p) => e.interpreter.happensBeforeInvoc(e.state, p._1, p._2), Pair(argsT(0), argsT(1)))
                }

              case BuiltInFunc.BF_sameTransaction() =>
                Eq(env((e, a: CallId) => e.state.calls(a).callTransaction, argsT[CallId](0)),
                  env((e, a: CallId) => e.state.calls(a).callTransaction, argsT[CallId](1)))
              case BuiltInFunc.BF_less() =>
                op[(BigInt, BigInt), Boolean](e => e._1 < e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_lessEq() =>
                op[(BigInt, BigInt), Boolean](e => e._1 <= e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_greater() =>
                op[(BigInt, BigInt), Boolean](e => e._1 > e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_greaterEq() =>
                op[(BigInt, BigInt), Boolean](e => e._1 >= e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_equals() =>
                Eq(argsT(0), argsT(1))
              case BuiltInFunc.BF_notEquals() =>
                Neg(Eq(argsT(0), argsT(1)))
              case BuiltInFunc.BF_and() =>
                And(argsT(0), argsT(1))
              case BuiltInFunc.BF_or() =>
                Neg(And(Neg(argsT(0)), Neg(argsT(1))))
              case BuiltInFunc.BF_implies() =>
                Neg(And(argsT(0), Neg(argsT(1))))
              case BuiltInFunc.BF_iff() =>
                Eq(argsT(0), argsT(1))
              case BuiltInFunc.BF_not() =>
                Neg(argsT(0))
              case BuiltInFunc.BF_plus() =>
                op[(BigInt, BigInt), BigInt](e => e._1 + e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_minus() =>
                op[(BigInt, BigInt), BigInt](e => e._1 - e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_mult() =>
                op[(BigInt, BigInt), BigInt](e => e._1 * e._2, Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_div() =>
                op[(BigInt, BigInt), BigInt](e => MathUtils.euclideanDiv(e._1, e._2), Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_mod() =>
                op[(BigInt, BigInt), BigInt](e => MathUtils.euclideanMod(e._1, e._2), Pair(argsT(0), argsT(1)))
              case BuiltInFunc.BF_getOperation() =>
                env[CallId, DataTypeValue]((env, a) => env.state.calls(a).operation, argsT(0))
              case BuiltInFunc.BF_getInfo() =>
                env[InvocationId, DataTypeValue]((env, a) => env.state.invocations(a).operation, argsT(0))
              case BuiltInFunc.BF_getResult() =>
                env[InvocationId, DataTypeValue]((env, a) => env.state.invocations(a).result.getOrElse(DataTypeValue("NoResult", List())), argsT(0))
              case BuiltInFunc.BF_getOrigin() =>
                args.head.getTyp match {
                  case CallIdType() =>
                    env[CallId, InvocationId]((env, a) => env.state.calls(a).origin, argsT(0))
                  case TransactionIdType() =>
                    env[TransactionId, InvocationId]((env, a) => env.state.transactions(a).origin, argsT(0))
                  case other => unexpected(other)
                }

              case BuiltInFunc.BF_getTransaction() =>
                env[CallId, TransactionId]((env, a) => env.state.calls(a).callTransaction, argsT(0))
              case BuiltInFunc.BF_inCurrentInvoc() =>
                env[CallId, Boolean]((env, a) => env.localState.currentInvoc.contains(env.state.calls(a).origin), argsT(0))
              case BuiltInFunc.BF_distinct() =>
                if (args.isEmpty)
                  ConstExpr(true)
                else {
                  val argsT: List[Expr[Any]] = args.map(translateExpr)
                  val neqs: Iterator[Expr[Boolean]] =
                    for {
                      as <- argsT.tails
                      if as.nonEmpty
                      a = as.head
                      b <- as.tail
                    } yield Neg(Eq(a, b))
                  neqs.reduce(And)
                }
            }
        }
      case TypedAst.CrdtQuery(source, typ, qryOp) =>
        env[DataTypeValue, Any]((e, a) => e.interpreter.evaluateQueryOp(e.localState, e.state, a), translateExpr(qryOp))(implicitly[Type[DataTypeValue]], translateType(typ))
      case TypedAst.QuantifierExpr(source, quantifier, vars, expr) =>
        val newVars: Map[String, Var[_]] = vars.map(v => v.name.name -> Var(v.name.name)).toMap
        val newCtxt = ctxt.copy(vars = ctxt.vars ++ newVars)
        var res = translateExpr[Boolean](expr)(newCtxt)
        if (quantifier == InputAst.Exists())
          res = Neg(res)

        for (v <- vars.reverse) {
          res = Forall(newVars(v.name.name), translateType(v.typ), res)
        }
        if (quantifier == InputAst.Exists())
          res = Neg(res)
        res
      case ae@TypedAst.AggregateExpr(source, op, vars, filter, elem) =>
        // we evaluate this, by extracting the free variables to a tuple
        // and then using the default interpreter
        val freeVars: List[VarUse] = ae.freeVars().toList

        val unit: Expr[Any] = ConstExpr(())(T_Unit).asInstanceOf[Expr[Any]]

        def toTuple(list: List[VarUse]): (List[(Any => Any)], Expr[Any]) =
          list match {
            case List() => (List(), unit)
            case x::xs =>
              val (xsL, xsV) = toTuple(xs)

              val xsL2 = xsL.map(c => (e: Any) => c(e.asInstanceOf[(Any,Any)]))
              val v = Pair(translateExpr(x), xsV)
              val xL = (e: Any) => e.asInstanceOf[(Any,Any)]._1
              (xL::xsL2, v.asInstanceOf[Expr[Any]])
          }

        val (extract, freeVarsE) = toTuple(freeVars)
        def compute(e: InterpreterEnv, x: Any): BigInt = {
          val newVarValues =
            for ((v, e) <- freeVars.zip(extract)) yield
              Interpreter.LocalVar(v.name) -> AnyValue(e(x))
          val ls = e.localState.copy(
            varValues = e.localState.varValues ++ newVarValues
          )

          e.interpreter.evalExpr(ae, ls, e.state)(e.interpreter.defaultAnyValueCreator).intValue()
        }
        env(compute, freeVarsE)(T_Any, T_Int)
      case TypedAst.InAllValidSnapshots(source, expr) =>
        ???
    }

  private def bool(value: Boolean) = {
    ConstExpr(value)(BoolType())
  }

  def op[A, R](f: A => R, arg: Expr[A])(implicit at: Type[A], rt: Type[R]): Expr[R] =
    Opaque[A, R](at, rt, (env, a) => f(a), arg)

  def env[R](f: InterpreterEnv => R)(implicit rt: Type[R]): Expr[R] =
    Opaque[Unit, R](T_Unit, rt, (env, a) => f(env.asInstanceOf[InterpreterEnv]), ConstExpr(()))

  def env[A, R](f: (InterpreterEnv, A) => R, arg: Expr[A])(implicit at: Type[A], rt: Type[R]): Expr[R] =
    Opaque[A, R](at, rt, (env, a) => f(env.asInstanceOf[InterpreterEnv], a), arg)

  //  def state[T](f: Interpreter.State => T)(implicit rt: Type[T]): Expr[T] =
  //    Opaque(StateType, rt, { case List(s: Interpreter.State) => f(s) }, Var("$state"))
  //
  //  def localState[T](f: Interpreter.LocalState => T)(implicit rt: Type[T]): Expr[T] =
  //    Opaque(LocalStateType, rt, { case List(s: Interpreter.LocalState) => f(s) }, Var("$localState"))


}
