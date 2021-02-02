package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.{TypedAst, crdts}
import crdtver.language.TypedAst.{FunctionKind, InExpr, TypeVarUse}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.{QueryStructure, printTypes}


class AFileAccessRightsCrdt extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val AFileAccessRightsQry = "AFileAccessRightsQry"

  private val RegisterOp = "RegisterOp"

  private val Assign = "Assign"

  private val ReadPermsUB = "ReadPermsUB"

  private val ReadPermsLB = "ReadPermsLB"


  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      RegisterOp,
      List("T"),
      List(
        dtCase(Assign, List("value" -> TypeVarUse("T")())),
      )
    ),
    dataType(AFileAccessRightsQry, List("T"),List(
      dtCase(ReadPermsUB, List()),
      dtCase(ReadPermsLB, List())
    )))

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def toString: String = s"${AFileAccessRightsCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

    val T: TypedAst.InTypeExpr = typeArgs.head

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(RegisterOp, List(T))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(AFileAccessRightsQry, List(T))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ReadPermsUB, List()) => T                                                                                                                                                                                                                                                                                                              
      case QueryStructure(ReadPermsLB, List()) => T
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclEnsures(ReadPermsUB, List(), T, {
        val result = varUse("result", T)
        val result2 = varUse("result2", T)
        val upperBound = varUse("result", T)
        val someValue = varUse("someValue", T)
        val someValue2 = varUse("someValue2", T)
        val someAssign = varUse("someAssign", T)
        val someAssign2 = varUse("someAssign", T)


        val relevantCall = varUse("relevantCall")
        val relevantCall2 = varUse("relevantCall2")
        val otherCall = varUse("otherCall")
        val otherCall2 = varUse("otherCall2")


        forall(relevantCall, forall(someAssign,
            (   relevantCall.isVis
                && relevantCall.op === makeOp(Assign, someAssign)
                && not(exists(otherCall, exists(someValue, otherCall.isVis && relevantCall < otherCall && otherCall.op === makeOp(Assign, someValue))))
                // all uninterrupted Assigns to the register
            ) -->
            (
                (_upperBoundedByOrEq(someAssign, result))// upper bound 
            )
        )) &&
        (
        forall(result2,
            (
                forall(relevantCall2, forall(someAssign2,
                (
                    relevantCall2.isVis
                    && relevantCall2.op === makeOp(Assign, someAssign2)
                    && not(exists(otherCall2, exists(someValue2, otherCall2.isVis && relevantCall2 < otherCall2 && otherCall2.op === makeOp(Assign, someValue2))))
                ) -->
                (
                    _upperBoundedByOrEq(someAssign2, result2)
                )
                ))
            ) -->
            (
                _upperBoundedByOrEq(result, result2)
            ))
        )
      }),
      queryDeclEnsures(ReadPermsLB, List(), T, {
        val result = varUse("result", T)
        val result2 = varUse("result2", T)
        val upperBound = varUse("result", T)
        val someValue = varUse("someValue", T)
        val someValue2 = varUse("someValue2", T)
        val someAssign = varUse("someAssign", T)
        val someAssign2 = varUse("someAssign", T)


        val relevantCall = varUse("relevantCall")
        val relevantCall2 = varUse("relevantCall2")
        val otherCall = varUse("otherCall")
        val otherCall2 = varUse("otherCall2")

        forall(relevantCall, forall(someAssign,
            (   relevantCall.isVis
                && relevantCall.op === makeOp(Assign, someAssign)
                && not(exists(otherCall, exists(someValue, otherCall.isVis && relevantCall < otherCall && otherCall.op === makeOp(Assign, someValue))))
                // all uninterrupted Assigns to the register
            ) -->
            (
                (_lowerBoundedByOrEq(someAssign, result))// upper bound 
            )
        )) &&
        (
        forall(result2,
            (
                forall(relevantCall2, forall(someAssign2,
                (
                    relevantCall2.isVis
                    && relevantCall2.op === makeOp(Assign, someAssign2)
                    && not(exists(otherCall2, exists(someValue2, otherCall2.isVis && relevantCall2 < otherCall2 && otherCall2.op === makeOp(Assign, someValue2))))
                ) -->
                (
                    _lowerBoundedByOrEq(someAssign2, result2)
                )
                ))
            ) -->
            (
                _lowerBoundedByOrEq(result, result2)
            ))
        )
      })
    )

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = AFileAccessRightsCrdt.this.additionalDataTypes

    def _upperBoundedByOrEq(val1: InExpr, val2: InExpr): InExpr = (
      (
          val1 === mkDatatypeCase("ANone", T) &&
          (val2 === mkDatatypeCase("ANone", T) || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) || val2 === mkDatatypeCase("ARW", T))
      ) ||
      (
          val1 === mkDatatypeCase("AR", T) &&
          (val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("ARW", T))
      ) ||
      (
          val1 === mkDatatypeCase("AW", T) &&
          (val2 === mkDatatypeCase("AW", T) || val2 === mkDatatypeCase("ARW", T))
      ) ||
      (
          val1 === mkDatatypeCase("ARW", T) &&
          val2 === mkDatatypeCase("ARW", T)
      ) ||
      (
          val1 === mkDatatypeCase("UNone", T) &&
          ( val2 === mkDatatypeCase("URW", T) || val2 === mkDatatypeCase("UR", T) || val2 === mkDatatypeCase("UW", T) || val2 === mkDatatypeCase("UNone", T)
            || val2 === mkDatatypeCase("ARW", T) || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("UR", T) &&
          ( val2 === mkDatatypeCase("UR", T) || val2 === mkDatatypeCase("URW", T) || val2 === mkDatatypeCase("ARW", T) 
          || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("UW", T) &&
          ( val2 === mkDatatypeCase("UW", T) || val2 === mkDatatypeCase("URW", T) || val2 === mkDatatypeCase("ARW", T) 
          || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("URW", T) &&
          ( val2 === mkDatatypeCase("URW", T) || val2 === mkDatatypeCase("ARW", T) 
          || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      )
    )

    def _lowerBoundedByOrEq(val1: InExpr, val2: InExpr): InExpr = (
      (
          val1 === mkDatatypeCase("ANone", T) &&
          val2 === mkDatatypeCase("ANone", T)
      ) ||
      (
          val1 === mkDatatypeCase("AR", T) &&
          (val2 === mkDatatypeCase("ANone", T) || val2 === mkDatatypeCase("AR", T))
      ) ||
      (
          val1 === mkDatatypeCase("AW", T) &&
          (val2 === mkDatatypeCase("ANone", T) || val2 === mkDatatypeCase("AW", T))
      ) ||
      (
          val1 === mkDatatypeCase("ARW", T) &&
          (val2 === mkDatatypeCase("ANone", T) || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) || val2 === mkDatatypeCase("ARW", T))
      ) ||
      (
          val1 === mkDatatypeCase("UNone", T) &&
          (val2 === mkDatatypeCase("UNone", T) || val2 === mkDatatypeCase("ARW", T) || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T)
            ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("UR", T) &&
          (val2 === mkDatatypeCase("UR", T) || val2 === mkDatatypeCase("UNone", T) || val2 === mkDatatypeCase("ARW", T) || val2 === mkDatatypeCase("AR", T)
            || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("UW", T) &&
          (val2 === mkDatatypeCase("UW", T) || val2 === mkDatatypeCase("UNone", T) || val2 === mkDatatypeCase("ARW", T) || val2 === mkDatatypeCase("AR", T)
            || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      ) ||
      (
          val1 === mkDatatypeCase("URW", T) &&
          ( val2 === mkDatatypeCase("URW", T) || val2 === mkDatatypeCase("UR", T) || val2 === mkDatatypeCase("UW", T) || val2 === mkDatatypeCase("UNone", T)
            || val2 === mkDatatypeCase("ARW", T) || val2 === mkDatatypeCase("AR", T) || val2 === mkDatatypeCase("AW", T) ||val2 === mkDatatypeCase("ANone", T))
      )
    )
  }

  /** name of the CRDT */
  override def name: String = "AFileAccessRights"
}

