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
    forall(List(v), exp)
  }

  def forall(v: VarUse, exp: InExpr): InExpr = {
    forall(List(InVariable(NoSource(), ident(v.name), v.typ)), exp)
  }

  def forall(vs: List[InVariable], exp: InExpr): InExpr = {
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

  def exists(v: InVariable, exp: InExpr): InExpr = {
    exists(List(v), exp)
  }

  def exists(v: VarUse, exp: InExpr): InExpr = {
    exists(List(InVariable(NoSource(), ident(v.name), v.typ)), exp)
  }

  def exists(vs: List[InVariable], exp: InExpr): InExpr = {
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
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_happensBefore(HappensBeforeOn.Call()),
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
    case _ => ApplyBuiltin(
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
    case _ => ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_implies(),
      args = List(exp1, exp2)
    )
  }

  def not(exp: InExpr): InExpr = exp match {
    case b: BoolConst => b.copy(value = !b.value)
    case _ => ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_not(),
      args = List(exp)
    )
  }

  def getOp(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = SomeOperationType(),
      function = BF_getOperation(),
      args = List(exp)
    )
  }

  def getOrigin(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationIdType(),
      function = BF_getOrigin(),
      args = List(exp)
    )
  }

  def getTransaction(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationIdType(),
      function = BF_getTransaction(),
      args = List(exp)
    )
  }

  def invocationInfo(exp: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = InvocationInfoType(),
      function = BF_getInfo(),
      args = List(exp)
    )
  }

  def isEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
    ApplyBuiltin(
      source = NoSource(),
      typ = BoolType(),
      function = BF_equals(),
      args = List(exp1, exp2)
    )
  }

  def notEquals(exp1: InExpr, exp2: InExpr): ApplyBuiltin = {
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
    else
      ApplyBuiltin(
        source = NoSource(),
        typ = BoolType(),
        function = BF_distinct(),
        args = args
      )
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

  def makeOperation(name: String, operationType: InTypeExpr, tArgs: List[InTypeExpr], exp: TypedAst.InExpr*): TypedAst.FunctionCall =
    makeOperationL(name, operationType, tArgs, exp.toList)

  def makeOperationL(name: String, operationType: InTypeExpr, tArgs: List[InTypeExpr], exp: List[InExpr]): FunctionCall =
    TypedAst.FunctionCall(
      source = NoSource(),
      typ = operationType,
      functionName = Identifier(NoSource(), name),
      typeArgs = tArgs,
      args = exp,
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