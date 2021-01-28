package crdtver.language.crdts

import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.{TypedAst, crdts}
import crdtver.language.TypedAst.{FunctionKind, InExpr, TypeVarUse}
import crdtver.language.TypedAstHelper._
import crdtver.language.crdts.ACrdtInstance.{QueryStructure, printTypes}


class MVRegisterCrdt extends CrdtTypeDefinition {

  /** number of normal type parameters */
  override def numberTypes: Int = 1

  /** number of CRDT type parameters */
  override def numberInstances: Int = 0

  private val RegisterOp = "RegisterOp"

  private val Assign = "Assign"

  private val MVRegisterQry = "MVRegisterQry"

  private val ReadFirst = "ReadFirst"

  private val ReadPermsUpper = "ReadPermsUpper"

  private val ReadPermsLower = "ReadPermsLower"

  private val MvContains = "MvContains"

  override def additionalDataTypes: List[TypedAst.InTypeDecl] = List(
    dataType(
      RegisterOp,
      List("T"),
      List(
        dtCase(Assign, List("value" -> TypeVarUse("T")())),
      )
    ),
    dataType(MVRegisterQry, List("T"),List(
      dtCase(ReadFirst, List()),
      dtCase(ReadPermsUpper, List()),
      dtCase(ReadPermsLower, List()),
      dtCase(MvContains, List("value" -> TypeVarUse("T")()))))
  )

  override def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance = new ACrdtInstance {

    override def toString: String = s"${MVRegisterCrdt.this.name}${printTypes(typeArgs, crdtArgs)}"

    val T: TypedAst.InTypeExpr = typeArgs.head

    override def operationType: TypedAst.InTypeExpr = TypedAst.SimpleType(RegisterOp, List(T))()

    override def queryType: TypedAst.InTypeExpr = TypedAst.SimpleType(MVRegisterQry, List(T))()

    override def queryReturnType(q: QueryStructure): TypedAst.InTypeExpr = q match {
      case QueryStructure(ReadFirst, List()) => T
      case QueryStructure(ReadPermsUpper, List()) => T                                                                                                                                                                                                                                                                                                              
      case QueryStructure(ReadPermsLower, List()) => T
    }

    override def queryDefinitions(): List[TypedAst.InQueryDecl] = List(
      queryDeclEnsures(ReadFirst, List(), T, {
        val result = varUse("result", T)
        val c = varUse("c")
        val c2 = varUse("c2")
        val v = varUse("v", T)
        not(exists(c, exists(v, c.isVis && c.op === makeOp(Assign, v)))) ||
          exists(c, c.isVis && c.op === makeOp(Assign, result)
            && not(exists(c2, exists(v, c2.isVis && c < c2 && c2.op === makeOp(Assign, v)))))
      }),
      queryDeclEnsures(ReadPermsUpper, List(), T, {
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

        // forall(relevantCall, forall(someAssign,
        //     (   relevantCall.isVis
        //         && relevantCall.op === makeOp(Assign, someAssign)
        //         && not(exists(otherCall, exists(someValue, otherCall.isVis && relevantCall < otherCall && otherCall.op === makeOp(Assign, someValue))))
        //         // all uninterrupted Assigns to the register
        //     ) -->
        //     (
        //         (_upperBoundedByOrEq(someAssign, result)) && // upper bound
        //         forall(upperBound,
        //             (_upperBoundedBy(someAssign, upperBound)) -->
        //             (_upperBoundedByOrEq(result, upperBound))
        //         ) // lowest upper bound
        //     )
        // ))

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
      queryDeclEnsures(ReadPermsLower, List(), T, {
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

        // forall(relevantCall, forall(someAssign,
        //     (   relevantCall.isVis
        //         && relevantCall.op === makeOp(Assign, someAssign)
        //         && not(exists(otherCall, exists(someValue, otherCall.isVis && relevantCall < otherCall && otherCall.op === makeOp(Assign, someValue))))
        //         // all uninterrupted Assigns to the register
        //     ) -->
        //     (
        //         (_upperBoundedByOrEq(someAssign, result)) && // upper bound
        //         forall(upperBound,
        //             (_upperBoundedBy(someAssign, upperBound)) -->
        //             (_upperBoundedByOrEq(result, upperBound))
        //         ) // lowest upper bound
        //     )
        // ))

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
      }),
      {
        val x = "x" :: new TypeExtensions(T)
        queryDeclImpl(MvContains, List(x), T, {
          val c = varUse("c")
          val c2 = varUse("c2")
          val v = varUse("v", T)
          exists(c, c.isVis && c.op === makeOp(Assign, varUse(x))
              && not(exists(c2, exists(v, c2.isVis && c < c2 && c2.op === makeOp(Assign, v)))))
        })
      }
    )

    override def additionalDataTypesRec: List[TypedAst.InTypeDecl] = MVRegisterCrdt.this.additionalDataTypes

    def _upperBoundedBy(val1: InExpr, val2: InExpr): InExpr = (
      (
          val1 === mkDatatypeCase("R") &&
          (val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("W") &&
          (val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("X") &&
          (val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("RW") &&
          (val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("RX") &&
          (val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("WX") &&
          (val2 === mkDatatypeCase("RWX"))
      )
    )

    /** datatype constructor of type T (type of elements in the register) */
    private def mkDatatypeCase(name: String, args: TypedAst.InExpr*): TypedAst.FunctionCall =
      TypedAst.FunctionCall(
            source = NoSource(),
            typ = T,
            functionName = Identifier(NoSource(), name),
            typeArgs = List(),
            args = args.toList,
            kind = FunctionKind.FunctionKindDatatypeConstructor()
          )

    def _upperBoundedByOrEq(val1: InExpr, val2: InExpr): InExpr = (
      (
        val1 === mkDatatypeCase("None") &&
        (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("R") || val2 === mkDatatypeCase("W") || val2 === mkDatatypeCase("X")
            || val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("R") &&
          (val2 === mkDatatypeCase("R") || val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("W") &&
          (val2 === mkDatatypeCase("W") || val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("X") &&
          (val2 === mkDatatypeCase("X") || val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("RW") &&
          (val2 === mkDatatypeCase("RW") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("RX") &&
          (val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("WX") &&
          (val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      ) ||
      (
          val1 === mkDatatypeCase("RWX") &&
          val2 === mkDatatypeCase("RWX")
      )
    )

    def _lowerBoundedByOrEq(val1: InExpr, val2: InExpr): InExpr = (
      (
          val1 === mkDatatypeCase("None") &&
          val2 === mkDatatypeCase("None")
      ) ||
      (
          val1 === mkDatatypeCase("R") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("R"))
      ) ||
      (
          val1 === mkDatatypeCase("W") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("W"))
      ) ||
      (
          val1 === mkDatatypeCase("X") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("X"))
      ) ||
      (
          val1 === mkDatatypeCase("RW") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("R") || val2 === mkDatatypeCase("W") || val2 === mkDatatypeCase("RW"))
      ) ||
      (
          val1 === mkDatatypeCase("RX") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("R") || val2 === mkDatatypeCase("X") || val2 === mkDatatypeCase("RX"))
      ) ||
      (
          val1 === mkDatatypeCase("WX") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("W") || val2 === mkDatatypeCase("X") || val2 === mkDatatypeCase("WX"))
      ) ||
      (
          val1 === mkDatatypeCase("RWX") &&
          (val2 === mkDatatypeCase("None") || val2 === mkDatatypeCase("R") || val2 === mkDatatypeCase("W") || val2 === mkDatatypeCase("X") || val2 === mkDatatypeCase("RW")
            || val2 === mkDatatypeCase("RX") || val2 === mkDatatypeCase("WX") || val2 === mkDatatypeCase("RWX"))
      )
    )
  }

  /** name of the CRDT */
  override def name: String = "MultiValueRegister"
}

