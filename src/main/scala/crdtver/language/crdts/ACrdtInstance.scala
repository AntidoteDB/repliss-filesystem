package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst
import crdtver.language.TypedAst._
import crdtver.language.crdts.ACrdtInstance.{Func, QueryStructure, QueryStructureLike}
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, State}


abstract class ACrdtInstance {

  def operationType: TypedAst.InTypeExpr

  def queryType: TypedAst.InTypeExpr

  def queryReturnType(qry: QueryStructure): TypedAst.InTypeExpr

  def queryDefinitions(): List[InQueryDecl]

  /**
   * Evaluates a query efficiently in the interpreter.
   *
   * If the return value is None the interpreter will fall back to evaluating the specification.
   */
  def evaluateQuery(name: String, args: List[AbstractAnyValue], state: State): Option[AnyValue] = None

  def additionalDataTypesRec: List[TypedAst.InTypeDecl]

  def toFlatQuery[T](fc: T)(implicit s: QueryStructureLike[T]): Option[Func[T]] =
    s.structure(fc)

  def makeOp(name: String, exp: TypedAst.InExpr*): TypedAst.FunctionCall =
    makeOpL(name, exp.toList)

  def makeOpL(name: String, exp: List[TypedAst.InExpr]): TypedAst.FunctionCall =
    TypedAst.FunctionCall(
      source = NoSource(),
      typ = CallInfoType(),
      functionName = Identifier(NoSource(), "Op"),
      typeArgs = List(),
      kind = FunctionKind.FunctionKindDatatypeConstructor(),
      args = List(
        makeOperationL(name, exp)
      )
    )


  def makeOperation(name: String, exp: TypedAst.InExpr*): TypedAst.FunctionCall = {
    makeOperationL(name, exp.toList)
  }

  private def makeOperationL(name: String, exp: List[InExpr]): FunctionCall = {
    val tArgs = operationType.extractTypeArgs

    TypedAst.FunctionCall(
      source = NoSource(),
      typ = operationType,
      functionName = Identifier(NoSource(), name),
      typeArgs = tArgs,
      args = exp,
      kind = FunctionKind.FunctionKindDatatypeConstructor()
    )
  }

}

object ACrdtInstance {

  case class Func[A](name: String, args: List[A])

  trait QueryStructureLike[T] {
    def structure(t: T): Option[Func[T]]
  }

  implicit val typedAstExprIsQueryStructureLike: QueryStructureLike[TypedAst.InExpr] = new QueryStructureLike[TypedAst.InExpr] {
    override def structure(t: InExpr): Option[Func[InExpr]] = t match {
      case fc: FunctionCall =>
        Some(Func(fc.functionName.name, fc.args))
      case _ =>
        None
    }
  }


  case class QueryStructure(name: String, args: List[QueryStructure])

}

