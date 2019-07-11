package crdtver.language.crdts

import crdtver.language.InputAst.BuiltInFunc.{BF_equals, BF_getOperation}
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.{TypedAst, Typer}
import crdtver.language.TypedAst.FunctionKind.FunctionKindDatatypeConstructor
import crdtver.language.TypedAst._
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.AbstractMapCrdt.{DeleteAffectsNothing, DeleteStrategy}
import crdtver.language.crdts.CrdtInstance.QueryImplementation
import crdtver.language.crdts.CrdtTypeDefinition._
import crdtver.parser.LangParser.TypeContext
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, CallInfo, DataTypeValue, State}
import crdtver.utils.{Err, Ok, Result}


object AbstractMapCrdt {

  sealed abstract class DeleteStrategy

  case class DeleteAffectsNothing() extends DeleteStrategy

  case class DeleteAffectsBefore() extends DeleteStrategy

  case class DeleteAffectsBeforeAndConcurrent() extends DeleteStrategy

}

case class MapCrdt(
  name: String,
  hasDelete: Boolean,
  deleteResets: DeleteStrategy,
  deleteStrategyForExists: DeleteStrategy,
) extends CrdtTypeDefinition {


  class Instance(val scope: String, keyType: InTypeExpr, valueType: CrdtInstance, context: NameContext) extends CrdtInstance()(context) {
    override implicit val nameContext: NameContext = context


    private val delete = context.newName("delete")

    private val update = context.newName("update")

    private val containsKey = context.newName("containsKey")

    private val read = context.newName("read")

    override val updateDatatypeName: UniqueName = context.newName(s"${scope}_update")

    override val queryDatatypeName: UniqueName = context.newName(s"${scope}_query")


    /** operations provided by this CRDT */
    override def operations: List[Operation] = {
      val queries = valueType.operations.filter(op => op.isQuery)
      val updates = valueType.operations.filter(op => op.isMutator)
      (if (hasDelete)
        List(
          SimpleOperation(this, delete, List(param("key", keyType)), TypeUnit()),
          SimpleOperation(this, containsKey, List(param("key", keyType)), BoolType()))
      else
        List()) ++
        List(
          SimpleOperation(this, update, List(param("key", keyType), param("nested_update", valueType.updateDatatype)),
            TypeUnit()
          ),
          SimpleOperation(this, read, List(param("key", keyType), param("nested_query", valueType.queryDatatype)),
            DependentReturnType(queries)),
        )
    }


    /** evaluates a query (for the interpreter) */
    override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {

      if (name == containsKey) {
        AnyValue(calculateExists(state, args.head))
      } else {
        // nested query
        val filteredCalls: Map[CallId, CallInfo] = filterCalls(state, args.head, deleteResets)
        val newState = state.copy(calls = filteredCalls)
        valueType.evaluateQuery(name, args.tail, newState)
      }
    }

    /*
    /** returns the query definitions for this CRDT */
    override def queryDefinitions: List[InQueryDecl] = {
      var queryDeclList = List[InQueryDecl]()
      val existsQuery: InQueryDecl = makeExistsQuery()
      queryDeclList = queryDeclList :+ existsQuery
      for (nQuery <- valueType.queryDefinitions) { // the queryDefinition method of the CrdtArg//
        val updateList = makeVariable("id", keyType) +: nQuery.params // Append the id of Mapcrdt
        nQuery.implementation match {
          case Some(x) =>
            val updatedExpr = rewriteQuery(x)
            val newQuery = nQuery.copy(implementation = Some(updatedExpr), params = updateList)
            queryDeclList = queryDeclList :+ newQuery
          case None =>
        }
        nQuery.ensures match {
          case Some(x) =>
            val updatedExpr = rewriteQuery(x)
            val newQuery = nQuery.copy(ensures = Some(updatedExpr), params = updateList)
            queryDeclList = queryDeclList :+ newQuery
          case None =>
        }
      }
      queryDeclList
    }




    private def makeExistsQuery(): InQueryDecl = {
      val c1 = varUse("c1")
      val c2 = varUse("c2")
      val callId1 = makeVariable("c1", CallIdType())
      val callId2 = makeVariable("c2", CallIdType())
      val key = varUse("key")
      val impl = deleteStrategyForExists match {
        case DeleteAffectsNothing() =>
          // exists c1 :: isUpdateOperation(c1, key)
          isExists(callId1, isUpdateOperation(c1, key))
        case AbstractMapCrdt.DeleteAffectsBefore() =>
          // exists c1 :: isUpdateOperation(c1, key)
          //  && !exists c2 :: isDeleteOperation(c2, key) && c1 happensBefore c2
          isExists(callId1, and(isUpdateOperation(c1, key),
            not(isExists(callId2, and(isDeleteOperation(c2, key), happensBeforeCall(c1, c2))))))
        case AbstractMapCrdt.DeleteAffectsBeforeAndConcurrent() =>
          // exists c1 :: isUpdateOperation(c1, key)
          //  && forall c2 :: isDeleteOperation(c2, key) --> c2 happensBefore c1
          isExists(callId1, and(isUpdateOperation(c1, key),
            forall(callId2, implies(isDeleteOperation(c2, key), happensBeforeCall(c2, c1)))))
      }

      InQueryDecl(
        source = NoSource(),
        name = Identifier(NoSource(), containsKey.toString),
        params = List(makeVariable("key", keyType)),
        returnType = BoolType(),
        ensures = None,
        implementation = Some(impl),
        annotations = Set()
      )
    }
 */
    /** checks if call c is an update operation on the given key  */
    private def isDeleteOperation(c: VarUse, key: InExpr) = {
      and(isVisible(c), isEquals(getOp(c), makeOperation(delete, key)))
    }

    /** checks if call c is an update operation on the given key  */
    private def isUpdateOperation(c: VarUse, key: InExpr): InExpr = {
      val argsVar = makeVariableU("nestedUpdate", valueType.updateDatatype)
      val args = varUse(argsVar.name)
      and(isVisible(c), isExists(argsVar, isEquals(getOp(c), makeOperation(update, key, args))))
    }



    /** transforms the calls to include only the nested operations  */
    private def filterCalls(state: State, key: AbstractAnyValue, deleteStrategy: DeleteStrategy): Map[CallId, CallInfo] = {
      var filtercalls = Map[CallId, CallInfo]()
      for (call <- state.calls.values) {
        val opName = call.operation.operationName
        val opKey = call.operation.args.head
        if (opKey == key && opName == update) {
          val nestedOperation = call.operation.args(1).value.asInstanceOf[DataTypeValue]
          filtercalls += (call.id -> call.copy(operation = nestedOperation))
        }
      }
      if (deleteStrategy != DeleteAffectsNothing()) {
        // remove all calls that have been affected by a delete:
        for (c <- state.calls.values) {
          if (c.operation.operationName == delete) {
            filtercalls = filtercalls.filter { case (k, v) =>
              val deletedKey = c.operation.args.head
              key != deletedKey || (deleteStrategy match {
                case AbstractMapCrdt.DeleteAffectsBefore() =>
                  !v.happensBefore(c)
                case AbstractMapCrdt.DeleteAffectsBeforeAndConcurrent() =>
                  c.happensBefore(v)
                case DeleteAffectsNothing() =>
                  true
              })
            }
          }
        }
      }
      filtercalls
    }

    def calculateExists(state: State, key: AbstractAnyValue): Boolean = {
      filterCalls(state, key, deleteStrategyForExists).nonEmpty
    }

    private def containsKeySpec(key: InExpr): InExpr = {
      val callId1 = makeVariableU("c1", CallIdType())
      val callId2 = makeVariableU("c2", CallIdType())
      val c1 = varUse(callId1.name)
      val c2 = varUse(callId2.name)
      deleteStrategyForExists match {
        case DeleteAffectsNothing() =>
          // exists c1 :: isUpdateOperation(c1, key)
          isExists(callId1, isUpdateOperation(c1, key))
        case AbstractMapCrdt.DeleteAffectsBefore() =>
          // exists c1 :: isUpdateOperation(c1, key)
          //  && !exists c2 :: isDeleteOperation(c2, key) && c1 happensBefore c2
          isExists(callId1, and(isUpdateOperation(c1, key),
            not(isExists(callId2, and(isDeleteOperation(c2, key), happensBeforeCall(c1, c2))))))
        case AbstractMapCrdt.DeleteAffectsBeforeAndConcurrent() =>
          // exists c1 :: isUpdateOperation(c1, key)
          //  && forall c2 :: isDeleteOperation(c2, key) --> c2 happensBefore c1
          isExists(callId1, and(isUpdateOperation(c1, key),
            forall(callId2, implies(isDeleteOperation(c2, key), happensBeforeCall(c2, c1)))))
      }
    }

    /*

        /**
          * rewrites the given query.
          *
          * this replaces every subexpression of the form:
          *
          * c.op == someUpdate
          *
          * to
          *
          * c.op == mapUpdate(id, someUpdate)
          * && (not exists d :: d.op == delete(id) && c1 happensBefore d)
          *
          **/
        private def rewriteQuery(x: InExpr): InExpr = {
          x match {
            case ApplyBuiltin(s, t, BF_equals(), List(
            ApplyBuiltin(s1, t1, BF_getOperation(), List(c1)),
            fc)) =>
              val newfc = rewriteQuery(fc)
              val d = varUse("d")
              val deleteId = makeVariable("d", CallIdType())
              val id = varUse("id")
              val notDeleted = deleteResets match {
                case DeleteAffectsNothing() =>
                  BoolConst(NoSource(), BoolType(), true)
                case AbstractMapCrdt.DeleteAffectsBefore() =>
                  not(isExists(deleteId, and(List(isEquals(getOp(d), makeOperation(delete, id)),
                    happensBeforeCall(c1, d)))))
                case AbstractMapCrdt.DeleteAffectsBeforeAndConcurrent() =>
                  forall(deleteId, and(List(isEquals(getOp(d), makeOperation(delete, id)),
                    happensBeforeCall(d, c1))))
              }


              val newExpr = and(ApplyBuiltin(s, t, BF_equals(), List(
                ApplyBuiltin(s1, t1, BF_getOperation(), List(c1)),
                newfc)), notDeleted)
              newExpr
            case i: IntConst =>
              i
            case v: VarUse =>
              v
            case b: BoolConst =>
              b
            case a: ApplyBuiltin => // Logical operators, Ex: a && b
              val updatedArgs = a.args.map(arg => rewriteQuery(arg)) // call updateExpr on each expr. (updateExpr(a), updateExpr(b))
              a.copy(args = updatedArgs)
            case f: FunctionCall =>
              if (operations.exists(op => op.name.toString == f.functionName.name)) {
                val id = varUse("id")
                FunctionCall(f.source, SimpleType(update.toString)(), Identifier(NoSource(), update.toString),
                  List(id, f), FunctionKindDatatypeConstructor())

              } else {
                f
              }
            case qe: QuantifierExpr =>
              val nextExpr = rewriteQuery(qe.expr)
              qe.copy(expr = nextExpr)
          }
        }

         */
    override def querySpecification(name: UniqueName, args: List[InExpr]): CrdtInstance.QuerySpecification = {
      if (name == containsKey) {
        /*
          Problem: Do I only support the case where the argument already has the form of a nested operation
          Or do I allow any kind of query?
          --> Fow now take the easy way and assume that the
          */
        QueryImplementation(containsKeySpec(args(0)))
      } else if (name == read) {
        ???
      } else {
        ???
      }
    }


  }

  override def makeInstance(scope: String, typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], context: NameContext): Result[CrdtInstance, String] = (typeArgs, crdtArgs) match {
    case (List(keyType), List(valueType)) =>
      Ok(new Instance(scope, keyType, valueType, context))

    case _ =>
      Err("Map datatype requires two type arguments: a key-type and a CRDT type for the value.")
  }


}

