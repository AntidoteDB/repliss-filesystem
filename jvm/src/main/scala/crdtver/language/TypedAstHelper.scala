package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.{Exists, Forall, Identifier, NoSource}
import crdtver.language.TypedAst._


/**
 * Helper functions for case classes in InputAst
 */

object TypedAstHelper {

  /**
   * forall quantifier function
   */
  def forall(v: InVariable, exp: InExpr): InExpr = {
    forallL(List(v), exp)
  }

  def forall(v: VarUse, exp: InExpr): InExpr = {
    forallL(List(InVariable(NoSource(), ident(v.name), v.typ)), exp)
  }

  def forallL(vs: List[InVariable], exp: InExpr): InExpr = {
    if (vs.isEmpty)
      return exp

    if (exp.isInstanceOf[BoolConst])
      return exp

    QuantifierExpr(
      source = NoSource(),
      quantifier = Forall(),
      vars = vs,
      expr = exp
    )
  }

  def sum(vs: List[InVariable], filter: InExpr, elem: InExpr): InExpr =
    AggregateExpr(
      NoSource(),
      Sum(),
      vs,
      filter,
      elem
    )

  def intConst(i: BigInt): IntConst =
    IntConst(NoSource(), IntType(), i)


  def exists(v: InVariable, exp: InExpr): InExpr = {
    existsL(List(v), exp)
  }

  def exists(v: VarUse, exp: InExpr): InExpr = {
    existsL(List(InVariable(NoSource(), ident(v.name), v.typ)), exp)
  }

  def existsL(vs: List[InVariable], exp: InExpr): InExpr = {
    if (vs.isEmpty)
      return exp

    if (exp.isInstanceOf[BoolConst])
      return exp

    QuantifierExpr(
      source = NoSource(),
      quantifier = Exists(),
      vars = vs,
      expr = exp
    )
  }

  def isVisible(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_isVisible(),
      args = List(exp)
    )
  }

  def happensBeforeCall(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    require(exp1.getTyp == CallIdType())
    require(exp2.getTyp == CallIdType())
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_happensBefore(HappensBeforeOn.Call()),
      args = List(exp1, exp2)
    )
  }

  def upperBoundedBy(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_upperBoundedBy(),
      args = List(exp1, exp2)
    )
  }

  def and(exp1: InExpr, exp2: InExpr): InExpr = (exp1, exp2) match {
    case (BoolConst(_, _, true), x) => x
    case (x@BoolConst(_, _, false), _) => x
    case (x, BoolConst(_, _, true)) => x
    case (_, x@BoolConst(_, _, false)) => x
    case _ =>
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_and(),
        args = List(exp1, exp2)
      )
  }

  def calculateAnd(exp: List[InExpr]): InExpr = {
    if (exp.isEmpty)
      bool(true)
    else
      exp.reduceLeft(and)
  }

  def or(exp1: InExpr, exp2: InExpr): InExpr = (exp1, exp2) match {
    case (x@BoolConst(_, _, true), _) => x
    case (BoolConst(_, _, false), x) => x
    case (_, x@BoolConst(_, _, true)) => x
    case (x, BoolConst(_, _, false)) => x
    case _ =>
      require(exp1.getTyp == BoolType())
      require(exp2.getTyp == BoolType())
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_or(),
        args = List(exp1, exp2)
      )
  }

  def calculateOr(exp: Iterable[InExpr]): InExpr = {
    if (exp.isEmpty)
      bool(false)
    else
      exp.reduceLeft(or)
  }

  def bool(v: Boolean): BoolConst = {
    TypedAst.BoolConst(NoSource(), BoolType(), v)
  }

  def implies(exp1: InExpr, exp2: InExpr): InExpr = (exp1, exp2) match {
    case (BoolConst(_, _, true), x) => x
    case (x@BoolConst(_, _, false), _) => x.copy(value = true)
    case (_, x@BoolConst(_, _, true)) => x
    case (x, BoolConst(_, _, false)) => not(x)
    case _ =>
      require(exp1.getTyp == BoolType())
      require(exp2.getTyp == BoolType())
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_implies(),
        args = List(exp1, exp2)
      )
  }

  def not(exp: InExpr): InExpr = exp match {
    case b: BoolConst => b.copy(value = !b.value)
    case _ =>
      require(exp.getTyp == BoolType())
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_not(),
        args = List(exp)
      )
  }

  def getOp(exp: InExpr): ApplyBuiltin = {
    require(exp.getTyp == CallIdType())
    ApplyBuiltin(
      source = NoSource(),
      typ = CallInfoType(),
      function = BF_getOperation(),
      args = List(exp)
    )
  }

  def getOrigin(exp: InExpr): ApplyBuiltin = {
    require(exp.getTyp == CallIdType() || exp.getTyp == TransactionIdType(),
      s"Cannot get origin of ${exp.getTyp} ($exp)")
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationIdType(),
      function = BF_getOrigin(),
      args = List(exp)
    )
  }

  def getTransaction(exp: InExpr): ApplyBuiltin = {
    require(exp.getTyp == CallIdType())
    ApplyBuiltin(
      source = NoSource(),
      typ = TransactionIdType(),
      function = BF_getTransaction(),
      args = List(exp)
    )
  }

  def invocationInfo(exp: InExpr): ApplyBuiltin = {
    require(exp.getTyp == TypedAst.InvocationIdType(), s"Type $exp must be an invocation Id.")
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationInfoType(),
      function = BF_getInfo(),
      args = List(exp)
    )
  }

  def invocationResult(exp: InExpr): ApplyBuiltin = {
    require(exp.getTyp == TypedAst.InvocationIdType(), s"Type $exp must be an invocation Id.")
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationResultType(),
      function = BF_getResult(),
      args = List(exp)
    )
  }

  def isEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    require(exp1.getTyp == exp2.getTyp, s"Types ${exp1.getTyp} and ${exp2.getTyp} must be equal in $exp1 == $exp2.")
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_equals(),
      args = List(exp1, exp2)
    )
  }

  def notEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    require(exp1.getTyp == exp2.getTyp, s"Types $exp1 and $exp2 must be equal.")
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_notEquals(),
      args = List(exp1, exp2)
    )
  }

  def distinct(args: List[InExpr]): TypedAst.InExpr = {
    if (args.size < 2)
      bool(true)
    else {
      require(args.forall(_.getTyp == args.head.getTyp))
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_distinct(),
        args = args
      )
    }
  }


  /**
   * Create a inVariable instance
   *
   * @param c       - variable name
   * @param typExpr - type name
   */

  def getVariable(c: String, typExpr: InTypeExpr): InVariable = {
    InVariable(
      source = NoSource(),
      name = Identifier(NoSource(), c),
      typ = typExpr
    )
  }

  def varUse(v: InVariable): VarUse = {
    VarUse(
      source = NoSource(),
      typ = v.typ,
      name = v.name.name
    )
  }

  def varUse(c: String, t: InTypeExpr = CallIdType()): VarUse = {
    VarUse(
      source = NoSource(),
      typ = t,
      name = c
    )
  }

  /** datatype constructor of type T (type of elements in the register) */
  def mkDatatypeCase(name: String, dtype: TypedAst.InTypeExpr, args: TypedAst.InExpr*): TypedAst.FunctionCall =
    TypedAst.FunctionCall(
        source = NoSource(),
        typ = dtype,
        functionName = Identifier(NoSource(), name),
        typeArgs = List(),
        args = args.toList,
        kind = FunctionKind.FunctionKindDatatypeConstructor()
        )

  //  def makeOperation(name: String, operationType: InTypeExpr, tArgs: List[InTypeExpr], exp: TypedAst.InExpr*): TypedAst.FunctionCall =
  //    makeOperationL(name, operationType, tArgs, exp.toList)

  def makeOperationL(name: String, operationType: InTypeExpr, tArgs: List[InTypeExpr], exp: List[InExpr]): FunctionCall =
    TypedAst.FunctionCall(
      source = NoSource(),
      typ = CallInfoType(),
      functionName = Identifier(NoSource(), "Op"),
      typeArgs = List(),
      args = List(TypedAst.FunctionCall(
        source = NoSource(),
        typ = operationType,
        functionName = Identifier(NoSource(), name),
        typeArgs = tArgs,
        args = exp,
        kind = FunctionKind.FunctionKindDatatypeConstructor()
      )),
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )

  def dtVal(name: String, typ: InTypeExpr, args: List[InExpr]): FunctionCall =
    TypedAst.FunctionCall(
      source = NoSource(),
      typ = typ,
      functionName = Identifier(NoSource(), name),
      typeArgs = List(),
      args = args,
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )


  /** finds a unique name not used in existing variables */
  def uniqueName(name: String, existing: List[String]): String = {
    var i = 0

    def n: String = if (i == 0) name else s"${name}$i"

    while (existing contains n) {
      i += 1
    }
    n
  }


  def makeInvocationInfo(name: String, exp: List[InExpr]): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      typ = InvocationInfoType(),
      functionName = Identifier(NoSource(), name),
      typeArgs = List(),
      args = exp,
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )
  }

  def NoResult(): FunctionCall = {
    FunctionCall(
      source = NoSource(),
      typ = InvocationResultType(),
      functionName = Identifier(NoSource(), "NoResult"),
      typeArgs = List(),
      args = List(),
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )
  }

  def ident(n: String): Identifier = Identifier(NoSource(), n)

  def dataType(name: String, typeParams: List[String], cases: List[DataTypeCase]): TypedAst.InTypeDecl =
    InTypeDecl(NoSource(),
      isIdType = false,
      ident(name),
      typeParameters = typeParams.map(t => TypedAst.TypeParameter(NoSource(), ident(t))),
      cases)

  def dtCase(name: String, args: List[(String, InTypeExpr)]): DataTypeCase =
    DataTypeCase(NoSource(), ident(name), args.map(p => InVariable(NoSource(), ident(p._1), p._2)))

  def queryDeclImpl(name: String, params: List[InVariable], returnType: InTypeExpr, impl: InExpr): TypedAst.InQueryDecl =
    InQueryDecl(NoSource(), ident(name), params, returnType, Some(impl), None, Set())

  def queryDeclEnsures(name: String, params: List[InVariable], returnType: InTypeExpr, ensures: InExpr): TypedAst.InQueryDecl =
    InQueryDecl(NoSource(), ident(name), params, returnType, None, Some(ensures), Set())


  implicit class ExprExtensions(l: InExpr) {
    def ===(r: InExpr): InExpr =
      isEquals(l, r)

    def op: InExpr =
      getOp(l)

    def info: InExpr =
      invocationInfo(l)

    def result: InExpr =
      invocationResult(l)

    def origin: InExpr =
      getOrigin(l)

    def &&(r: InExpr): InExpr =
      and(l, r)

    def ||(r: InExpr): InExpr =
      or(l, r)

    def -->(r: InExpr): InExpr =
      implies(l, r)

    def <(r: InExpr): InExpr =
      happensBeforeCall(l, r)

    def isVis: InExpr =
      isVisible(l)

  }

  implicit class TypeExtensions(t: InTypeExpr) {
    def ::(name: String): InVariable =
      TypedAst.InVariable(NoSource(), ident(name), t)

    def ::(name: VarUse): InVariable =
      TypedAst.InVariable(NoSource(), ident(name.name), t)
  }


}