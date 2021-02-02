package crdtver.language

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.BuiltInFunc.HappensBeforeOn.Unknown
import crdtver.parser.LangParser._
import crdtver.testing.Interpreter.AnyValue
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.language.implicitConversions

/**
  * This defines the abstract syntax of the Repliss input language in terms of case classes.
  */
object InputAst {

  sealed abstract class AstElem(source: SourceTrace) {

    def getSource(): SourceTrace = source

    override def toString: String = customToString

    def customToString: String
  }

  case class InProgram(
    name: String,
    source: ProgramContext,
    procedures: List[InProcedure],
    types: List[InTypeDecl],
    operations: List[InOperationDecl],
    queries: List[InQueryDecl],
    axioms: List[InAxiomDecl],
    invariants: List[InInvariantDecl],
    crdts: List[InCrdtDecl]
  ) extends AstElem(source) {
    override def customToString: String = "program"


    def findProcedure(procname: String): InProcedure =
      procedures.find(p => p.name.name == procname)
        .getOrElse(throw new RuntimeException(s"Procedure $procname not found."))

    def findQuery(queryName: String): Option[InQueryDecl] =
      queries.find(p => p.name.name == queryName)

    def findType(name: String): Option[InTypeDecl] =
      types.find(t => t.name.name == name)

    def findDatatype(name: String): Option[InTypeDecl] =
      findType(name).find(t => t.dataTypeCases.nonEmpty)

  }

  sealed abstract class InDeclaration(source: SourceTrace) extends AstElem(source: SourceTrace) {

  }

  case class InProcedure(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable],
    locals: List[InVariable],
    returnType: InTypeExpr,
    body: InStatement
  ) extends InDeclaration(source) {
    override def customToString: String = s"procedure $name"
  }

  case class InTypeDecl(
    source: SourceTrace,
    isIdType: Boolean,
    name: Identifier,
    typeParameters: List[TypeParameter],
    dataTypeCases: List[DataTypeCase]
  ) extends InDeclaration(source) {
    override def customToString: String = s"type $name"

  }

  case class TypeParameter(
    source: SourceTrace,
    name: Identifier
  ) extends AstElem(source: SourceTrace) {
    override def customToString: String = name.name
  }

  case class DataTypeCase(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends AstElem(source) {
    override def customToString: String = s"datatype case $name"
  }


  case class InOperationDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable]
  ) extends InDeclaration(source) {
    override def customToString: String = s"operation $name"
  }

  case class InQueryDecl(
    source: SourceTrace,
    name: Identifier,
    params: List[InVariable],
    returnType: InTypeExpr,
    implementation: Option[InExpr],
    ensures: Option[InExpr],
    annotations: Set[InAnnotation]
  ) extends InDeclaration(source) {
    override def customToString: String = s"query $name"
  }

  sealed trait InAnnotation

  case class InlineAnnotation() extends InAnnotation

  case class InAxiomDecl(
    source: SourceTrace,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: String = s"axiom $expr"
  }

  case class InInvariantDecl(
    source: SourceTrace,
    name: String,
    isFree: Boolean,
    expr: InExpr
  ) extends InDeclaration(source) {
    override def customToString: String = s"invariant $expr"
  }

  case class InCrdtDecl(
    source: SourceTrace,
    keyDecl: InKeyDecl
  ) extends InDeclaration(source) {
    override def customToString: String = s"crdt $keyDecl"
  }

  case class InKeyDecl(
    source: SourceTrace,
    name: Identifier,
    crdttype: InCrdtType)
    extends AstElem(source) {
    override def customToString: String = s"crdttype $crdttype"
  }

  sealed abstract class InCrdtType(source: SourceTrace)
    extends AstElem(source: SourceTrace) {
  }

  case class InCrdt(
    source: SourceTrace,
    name: Identifier,
    typ: List[InCrdtType]
  ) extends InCrdtType(source) {
    override def customToString: String = s"typ $name $typ"
  }

  case class InStructCrdt(
    source: SourceTrace,
    keyDecl: List[InKeyDecl]
  ) extends InCrdtType(source) {
    override def customToString: String = s"key $keyDecl"
  }

  case class SourceRange(start: SourcePosition, stop: SourcePosition) {
    override def toString: String = s"$start-$stop"
  }

  case class SourcePosition(line: Int, column: Int) {
    override def toString: String = s"$line:$column"
  }

  implicit def tokenToSourcePosition(t: Token): SourcePosition = {
    SourcePosition(t.getLine, t.getCharPositionInLine)
  }

  sealed abstract class SourceTrace {
    def stop: SourcePosition

    def start: SourcePosition

    def range: SourceRange = SourceRange(start, stop)

    def getLine: Int = start.line
  }

  case class ParserRuleSource(source: ParserRuleContext) extends SourceTrace {

    override def stop: SourcePosition = SourcePosition(source.stop.getLine, source.stop.getCharPositionInLine + source.stop.getText.length)

    override def start: SourcePosition = source.start
  }

  implicit def parserRuleContextToSourceTrace(source: ParserRuleContext): SourceTrace = ParserRuleSource(source)

  case class TokenSource(source: Token) extends SourceTrace {

    override def stop: SourcePosition = SourcePosition(source.getLine, source.getCharPositionInLine + source.getText.length)

    override def start: SourcePosition = source
  }

  implicit def parserRuleContextToSourceTrace(source: Token): SourceTrace = TokenSource(source)

  case class NoSource() extends SourceTrace {
    override def getLine: Int = 0

    override def stop: SourcePosition = SourcePosition(0, 0)

    override def start: SourcePosition = SourcePosition(0, 0)
  }

  case class Identifier(source: SourceTrace, name: String) extends AstElem(source) {
    override def customToString: String = name
  }

  case class InVariable(
    source: SourceTrace,
    name: Identifier,
    typ: InTypeExpr)
    extends AstElem(source) {
    override def customToString: String = s"var $name: $typ"
  }

  sealed abstract class InExpr(source: SourceTrace)
    extends AstElem(source: SourceTrace) {
  }

  case class VarUse(
    source: SourceTrace,
    name: String
  ) extends InExpr(source) {
    override def customToString: String = name
  }

  case class BoolConst(
    source: SourceTrace,
    value: Boolean
  ) extends InExpr(source) {
    override def customToString: String = value.toString
  }

  case class IntConst(
    source: SourceTrace,
    value: BigInt
  ) extends InExpr(source) {
    override def customToString: String = value.toString
  }


  sealed abstract class CallExpr(
    source: SourceTrace,
    args: List[InExpr]
  ) extends InExpr(source)

  case class FunctionCall(
    source: SourceTrace,
    functionName: Identifier,
    args: List[InExpr]
  ) extends CallExpr(source, args) {
    override def customToString: String = s"$functionName(${args.mkString(", ")})"
  }


  case class ApplyBuiltin(
    source: SourceTrace,
    function: BuiltInFunc,
    args: List[InExpr]
  ) extends CallExpr(source, args) {
    override def customToString: String = {
      function match {
        case BF_isVisible() => s"${args.head} is visible"
        case BF_happensBefore(_) => s"(${args.head} happens before ${args(1)})"
        case BF_upperBoundedBy() => s"(${args.head} upper bounded by ${args(1)})"
        case BF_sameTransaction() => s"sameTransaction(${args(0)}, ${args(1)})"
        case BF_less() => s"(${args.head} < ${args(1)})"
        case BF_lessEq() => s"(${args.head} <= ${args(1)})"
        case BF_greater() => s"(${args.head} > ${args(1)})"
        case BF_greaterEq() => s"(${args.head} >= ${args(1)})"
        case BF_equals() => s"(${args.head} == ${args(1)})"
        case BF_notEquals() => s"(${args.head} != ${args(1)})"
        case BF_and() => s"(${args.head} && ${args(1)})"
        case BF_or() => s"(${args.head} || ${args(1)})"
        case BF_implies() => s"(${args.head} ==> ${args(1)})"
        case BF_iff() => s"(${args.head} <==> ${args(1)})"

        case BF_plus() => s"(${args.head} + ${args(1)})"
        case BF_minus() => s"(${args.head} - ${args(1)})"
        case BF_mult() => s"(${args.head} * ${args(1)})"
        case BF_div() => s"(${args.head} / ${args(1)})"
        case BF_mod() => s"(${args.head} % ${args(1)})"

        case BF_not() => s"!(${args.head})"
        case BF_getOperation() => s"${args.head}.op"
        case BF_getInfo() => s"${args.head}.info"
        case BF_getResult() => s"${args.head}.result"
        case BF_getOrigin() => s"${args.head}.origin"
        case BF_getTransaction() => s"{args.head}.transaction"
        case BF_inCurrentInvoc() => s"${args.head}.inCurrentInvoc"
        case BF_distinct() => s"distinct(${args.map(_.customToString).mkString(", ")})"
      }
    }
  }

  case class QuantifierExpr(
    source: SourceTrace,
    quantifier: Quantifier,
    vars: List[InVariable],
    expr: InExpr
  ) extends InExpr(source) {
    override def customToString: String = s"($quantifier ${vars.mkString(", ")} :: $expr) "
  }


  sealed abstract class Quantifier {
    def isExists: Boolean

  }

  case class Forall() extends Quantifier {
    override def toString: String = "forall"

    override def isExists: Boolean = false
  }

  case class Exists() extends Quantifier {
    override def toString: String = "exists"

    override def isExists: Boolean = true
  }

  sealed abstract class BuiltInFunc

  object BuiltInFunc {

    case class BF_isVisible() extends BuiltInFunc()

    case class BF_happensBefore(on: HappensBeforeOn) extends BuiltInFunc()

    case class BF_upperBoundedBy() extends BuiltInFunc()

    sealed trait HappensBeforeOn

    object HappensBeforeOn {

      case class Unknown() extends HappensBeforeOn

      case class Call() extends HappensBeforeOn

      case class Invoc() extends HappensBeforeOn

    }

    case class BF_sameTransaction() extends BuiltInFunc()

    case class BF_less() extends BuiltInFunc()

    case class BF_lessEq() extends BuiltInFunc()

    case class BF_greater() extends BuiltInFunc()

    case class BF_greaterEq() extends BuiltInFunc()

    case class BF_equals() extends BuiltInFunc()

    case class BF_notEquals() extends BuiltInFunc()

    case class BF_and() extends BuiltInFunc()

    case class BF_or() extends BuiltInFunc()

    case class BF_implies() extends BuiltInFunc()

    case class BF_iff() extends BuiltInFunc()

    case class BF_not() extends BuiltInFunc()

    case class BF_plus() extends BuiltInFunc()

    case class BF_minus() extends BuiltInFunc()

    case class BF_mult() extends BuiltInFunc()

    case class BF_div() extends BuiltInFunc()

    case class BF_mod() extends BuiltInFunc()

    case class BF_getOperation() extends BuiltInFunc()

    case class BF_getInfo() extends BuiltInFunc()

    case class BF_getResult() extends BuiltInFunc()

    // get invocation for a call or transaction
    case class BF_getOrigin() extends BuiltInFunc()

    // get transaction for a call
    case class BF_getTransaction() extends BuiltInFunc()

    case class BF_inCurrentInvoc() extends BuiltInFunc()

    case class BF_distinct() extends BuiltInFunc()

  }


  sealed abstract class InStatement(source: SourceTrace)
    extends AstElem(source: SourceTrace) {

  }

  case class BlockStmt(
    source: SourceTrace,
    stmts: List[InStatement]
  ) extends InStatement(source) {
    override def customToString: String = s"{${stmts.mkString(";")}}"
  }

  def makeBlock(
    source: SourceTrace,
    stmts: List[InStatement]
  ): BlockStmt = BlockStmt(
    source,
    stmts.flatMap(flatten)
  )

  private def flatten(s: InStatement): List[InStatement] = s match {
    case BlockStmt(source, stmts) => stmts.flatMap(flatten)
    case _ => List(s)
  }

  case class Atomic(
    source: SourceTrace,
    body: InStatement
  ) extends InStatement(source) {
    override def customToString: String = s"atomic $body"
  }


  case class LocalVar(
    source: SourceTrace,
    variable: InVariable
  ) extends InStatement(source) {
    override def customToString: String = s"var $variable"
  }


  case class IfStmt(
    source: SourceTrace,
    cond: InExpr,
    thenStmt: InStatement,
    elseStmt: InStatement
  ) extends InStatement(source) {
    override def customToString: String = s"if ($cond) $thenStmt else $elseStmt"
  }

  case class MatchStmt(
    source: SourceTrace,
    expr: InExpr,
    cases: List[MatchCase]
  ) extends InStatement(source) {
    override def customToString: String = s"$expr match { ${cases.mkString(";")} }"
  }

  case class MatchCase(
    source: SourceTrace,
    pattern: InExpr,
    statement: InStatement
  ) extends AstElem(source) {

    override def customToString: String = s"case $pattern => $statement"
  }


  case class CrdtCall(
    source: SourceTrace,
    call: FunctionCall
  ) extends InStatement(source) {
    override def customToString: String = s"call $call"
  }


  case class Assignment(
    source: SourceTrace,
    varname: Identifier,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := $expr"
  }


  case class NewIdStmt(
    source: SourceTrace,
    varname: Identifier,
    typename: InTypeExpr
  ) extends InStatement(source) {
    override def customToString: String = s"$varname := new $typename"
  }


  case class ReturnStmt(
    source: SourceTrace,
    expr: InExpr,
    assertions: List[AssertStmt]
  ) extends InStatement(source) {
    override def customToString: String = s"return $expr"
  }


  case class AssertStmt(
    source: SourceTrace,
    expr: InExpr
  ) extends InStatement(source) {
    override def customToString: String = s"assert $expr"
  }


  sealed abstract class InTypeExpr(source: SourceTrace = NoSource())
    extends AstElem(source: SourceTrace) {
  }

  /** if no variable type is given it needs to be inferred */
  case class InferType() extends InTypeExpr {
    override def customToString: String = "infer"
  }


  case class FunctionType(argTypes: List[InTypeExpr], returnType: InTypeExpr)(source: SourceTrace = NoSource())
    extends InTypeExpr(source) {


    override def customToString: String = s"(${argTypes.mkString(", ")}) => $returnType"
  }


  case class UnresolvedType(name: String, typeArgs: List[InTypeExpr])(source: SourceTrace = NoSource()) extends InTypeExpr(source) {

    override def customToString: String = s"$name"
  }


}
