package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, CallIdType, Identifier, InQueryDecl, InTypeExpr}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param, SimpleOperation}
import crdtver.language.crdts.SetCrdt.{RemoveAffectsBefore, RemoveAffectsBeforeAndConcurrent, RemoveAffectsNothing, RemoveStrategy}
import crdtver.testing.Interpreter
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallId, State}
import crdtver.utils.{Err, Ok, Result}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object SetCrdt {

  sealed abstract class RemoveStrategy

  case class RemoveAffectsNothing() extends RemoveStrategy

  case class RemoveAffectsBefore() extends RemoveStrategy

  case class RemoveAffectsBeforeAndConcurrent() extends RemoveStrategy

}


case class SetCrdt(
  name: String,
  strategy: RemoveStrategy
) extends CrdtTypeDefinition {


  override def makeInstance(typeArgs: List[InTypeExpr], crdtArgs: List[CrdtInstance], crdtContext: CrdtContext): Result[CrdtInstance, String] = {
    (typeArgs, crdtArgs) match {
      case (List(elementType), List()) =>
        Ok(new CrdtInstance {

          private val add = crdtContext.newName("add")

          private val remove = crdtContext.newName("remove")

          private val contains = crdtContext.newName("contains")

          /** operations provided by this CRDT */
          override def operations: List[Operation] = List(
            SimpleOperation(add, List(Param("elem", elementType))),
            SimpleOperation(remove, List(Param("elem", elementType))),
            SimpleOperation(contains, List(Param("elem", elementType)), Some(BoolType()))
          )

          /** evaluates a query (for the interpreter) */
          override def evaluateQuery(name: UniqueName, args: List[AbstractAnyValue], state: State): AnyValue = {
            assert(name == contains)
            val elem = args.head
            val adds =
              state.calls.values.filter(c => {
                val op = c.operation
                op.operationName == add && op.args.head == elem
              })
            lazy val removes =
              state.calls.values.filter(c => {
                val op = c.operation
                op.operationName == add && op.args.head == elem
              })
            val res: Boolean = strategy match {
              case RemoveAffectsBefore() =>
                adds.exists(a => removes.forall(r => r.happensBefore(a)))
              case RemoveAffectsBeforeAndConcurrent() =>
                adds.exists(a => !removes.exists(r => r.happensAfter(a)))
              case RemoveAffectsNothing() =>
                adds.nonEmpty
            }
            AnyValue(res)
          }

          /** returns the query definitions for this CRDT */
          override def queryDefinitions: List[InQueryDecl] = {
            val c1 = varUse("c1")
            val c2 = varUse("c2")
            val callId1 = makeVariable("c1", CallIdType())
            val callId2 = makeVariable("c2", CallIdType())
            val args = varUse("args")
            val implementation = strategy match {
              case RemoveAffectsBefore() =>
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, args)),
                  forall(callId2, implies(and(isVisible(c2), isEquals(getOp(c2), makeOperation(remove, args))), happensBeforeCall(c2, c1))))))
              case RemoveAffectsBeforeAndConcurrent() =>
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, args)),
                  not(isExists(callId2, and(List(and(isVisible(c2), isEquals(getOp(c2), makeOperation(remove, args))), happensBeforeCall(c1, c2))))))))
              case RemoveAffectsNothing() =>
                isExists(callId1, and(List(isVisible(c1), isEquals(getOp(c1), makeOperation(add, args)))))
            }


            List(InQueryDecl(
              source = NoSource(),
              name = Identifier(NoSource(), contains.toString),
              params = List(makeVariable("args", elementType)),
              returnType = BoolType(),
              ensures = None,
              implementation = Some(
                implementation),
              annotations = Set()
            )
            )
          }

        })
      case _ =>
        Err("Set CRDT requires one type parameter.")
    }


  }


}