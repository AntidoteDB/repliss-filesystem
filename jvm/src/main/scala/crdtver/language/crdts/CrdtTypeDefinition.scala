package crdtver.language.crdts

import crdtver.language.TypedAst
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{BoolType, DataTypeCase, InExpr, InQueryDecl, InTypeDecl, InTypeExpr, InVariable, PrincipleType}
import crdtver.language.crdts.FlagCrdt._
import crdtver.testing.Interpreter.{AbstractAnyValue, AnyValue, CallInfo, State}

object CrdtTypeDefinition {

  case class Operation(
                        name: String,
                        params: List[Param]
                      ) {
    def paramTypes: List[InTypeExpr] = params.map(_.typ)
  }

  case class Param(name: String, typ: InTypeExpr)

  case class Query(
                    qname: String,
                    params: List[Param],
                    qreturnType: InTypeExpr
                  ) {
    def qparamTypes: List[InTypeExpr] = params.map(_.typ)
  }


  def makeParams(types: List[InTypeExpr], names: String*): List[Param] = {
    require(types.length == names.length, s"Unexpected number of types for $names: $types")
    names.zip(types).map { case (n, t) => Param(n, t) }.toList
  }

  //  /**
  //   * Append the parameters of crdt inside nested maps.
  //   * Map[ColumnId, Set_aw[TaskId] ], the operation becomes add(ColumnId, TaskId)
  //   */
  //  def operation(typeArgs: List[Param], crdtArgs: List[ACrdtInstance]): List[Operation] = {
  //    var operation = List[Operation]()
  //    val instance = crdtArgs.head
  //    for (op <- instance.operations()) yield {
  //      val structname = op.name
  //      val structtype: List[Param] = typeArgs ++ op.params
  //      operation = operation :+ Operation(structname, structtype)
  //    }
  //    val map_delete = Operation("delete", typeArgs)
  //    operation = operation :+ map_delete
  //    return operation
  //  }

  //  def query(typeArgs: List[InTypeExpr], crdtArgs: List[ACrdtInstance]): List[Query] = {
  //    var query = List[Query]()
  //    val instance = crdtArgs.head
  //    val keyParam = CrdtTypeDefinition.makeParams(typeArgs, "key")
  //    for (op <- instance.queries()) yield {
  //      val structname = op.qname
  //      val structtype = keyParam ++ op.params
  //      val returntype = op.qreturnType
  //      query = query :+ Query(structname, structtype, returntype)
  //    }
  //    query = query :+ Query("exists", keyParam, BoolType())
  //    return query
  //  }

  def latestCalls(state: State): List[CallInfo] = {
    (for {
      (c1, ci1) <- state.calls
      if !state.calls.exists { case (c2, ci2) => c1 != c2 && ci1.happensBefore(ci2) }
    } yield ci1).toList
  }

  val crdts: List[CrdtTypeDefinition] = List(
    new FlagCrdt(EW(), "Flag_ew"),
    new FlagCrdt(SEW(), "Flag_sew"),
    new FlagCrdt(DW(), "Flag_dw"),
    new FlagCrdt(SDW(), "Flag_sdw"),
    new SetCrdt(EW(), "Set_aw"),
    new SetCrdt(SEW(), "Set_saw"),
    new SetCrdt(DW(), "Set_rw"),
    new SetCrdt(SDW(), "Set_srw"),
    new MapCrdt(EW(), MapCrdt.DeleteAffectsPrior(), "Map_uw"),
    new MapCrdt(DW(), MapCrdt.DeleteAffectsPriorAndConcurrent(), "Map_dw"),
    new RegisterCrdt,
    new MVRegisterCrdt,
    new FileAccessRightsCrdt,
    new CounterCrdt
  )


}

abstract class CrdtTypeDefinition {

  /** name of the CRDT */
  def name: String

  /** number of normal type parameters */
  def numberTypes: Int

  /** number of CRDT type parameters */
  def numberInstances: Int

  def additionalDataTypes: List[TypedAst.InTypeDecl]

  def instantiate(typeArgs: List[TypedAst.InTypeExpr], crdtArgs: List[ACrdtInstance]): ACrdtInstance

}