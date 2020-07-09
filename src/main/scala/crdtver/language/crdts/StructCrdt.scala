package crdtver.language.crdts

import crdtver.language.InputAst.BuiltInFunc.{BF_equals, BF_getOperation, BF_isVisible}
import crdtver.language.TypedAst
import crdtver.language.TypedAst.{ApplyBuiltin, BoolType, FunctionCall, TypeVarUse}
import crdtver.language.TypedAstHelper.{TypeExtensions, _}
import crdtver.language.crdts.FlagCrdt.Strategy
import crdtver.language.crdts.MapCrdt.NestedOp
import org.graalvm.compiler.core.common.`type`.ArithmeticOpTable.BinaryOp.Add

class StructCrdt(structName: String, fields: Map[String, ACrdtInstance]) extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 0

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val Op = s"${name}Op"
  private val Query = s"${name}Query"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = {
    List(
      dataType(
        Op,
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(fieldName, List("nested" -> fieldInstance.operationType))
          ).toList
      ),
      dataType(
        Query,
        (for ((fieldName, fieldInstance) <- fields) yield
          dtCase(s"${fieldName}Qry", List("nested" -> fieldInstance.queryType))
          ).toList
      )
    )
  }

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(Op)()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(Query)()

    override def queryReturnType(queryName: String, queryArgs: List[TypedAst.InExpr]): TypedAst.InTypeExpr = queryName match {
      case s"${field}Qry" =>
        val instance = fields(field)
        queryArgs match {
          case List(FunctionCall(_, _, nestedQ, nestedArgs, _)) =>
            instance.queryReturnType(nestedQ.name, nestedArgs)
        }
    }

    /** rewrites nested query operation  */
    def rewriteNestedQry(field: String)(expr: TypedAst.InExpr): TypedAst.InExpr = {
      expr.rewrite {
        case ApplyBuiltin(_, _, BF_equals(), List(ApplyBuiltin(_, _, BF_getOperation(), List(c)), op)) =>
          c.op === makeOperation(s"${field}Qry", op)
      }
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = {
      // the queries in
      (for ((fieldName, fieldInstance) <- fields; fieldQry <- fieldInstance.queryDefinitions()) yield {
        fieldQry.rewrite(fieldName + "_" + fieldQry.name.name, rewriteNestedQry(fieldName))

      }).toList
    }

  }

  /** name of the CRDT */
  override def name: String = structName
}

