package crdtver.verification

import crdtver.language.InputAst.BuiltInFunc._
import crdtver.language.InputAst.{Identifier, NoSource}
import crdtver.language.TypedAst.{Assignment => _, FunctionCall => _, makeBlock => _, _}
import crdtver.language.crdts.CrdtTypeDefinition
import crdtver.language.crdts.CrdtTypeDefinition.{Operation, Param}
import crdtver.language.{AtomicTransform, InputAst, TypedAst}
import crdtver.verification
import crdtver.verification.WhyAst._

/**
 *
 * TODO noninterference check
 *
 */
class WhyTranslation(
  restrictDomains: Option[Int] = None,
  restrictCalls: Option[Int] = None,
  restrictInvocations: Option[Int] = None
) {

  private var types: Map[String, TypeDecl] = Map()
  //  var datatypeConstructors: List[FuncDecl] = List()
  private var stateVars: List[GlobalVariable] = List()

  private var queryFunctions: Map[String, LogicDecls] = Map()
  private var queryProcedures: List[AbstractFunction] = List()

  private var functionReplacements = Map[String, String]()

  private var invariants: List[Term] = List()
  private val callId: String = "callId"
  private val typeCallId = TypeSymbol(callId)
  private val invocationId: String = "invocationId"
  private val typeInvocationId = TypeSymbol(invocationId)
  private val transactionId: String = "transactionId"
  private val typeTransactionId = TypeSymbol(transactionId)
  private val invocationInfo: String = "invocationInfo"
  private val typeInvocationInfo = TypeSymbol(invocationInfo)
  private val invocationResult: String = "invocationResult"
  private val typeInvocationResult = TypeSymbol(invocationResult)
  private val operation: String = "operation"
  private val typeOperation = TypeSymbol(operation)

  private var newIdTypes: List[String] = List()

  private var operationDefs: Map[String, List[TypedParam]] = Map()

  private var procedures = List[InProcedure]()
  private var procedureNames = Set[String]()

  private var builtinFuncWrites = Map[String, List[Symbol]]()

  case class Context(
    procedureName: String = "no_procedure",
    procedureArgNames: List[Symbol] = List(),
    isInAtomic: Boolean = false,
    useOldCurrentInvocation: Boolean = false,
    refVars: Set[String] = Set(),
    targetIsLogic: Boolean = true
  ) {
    def isRefVar(varname: String): Boolean = refVars.contains(varname)

  }


  private val state_callops: String = "state_callOps"

  private val state_visiblecalls: String = "state_visibleCalls"

  private val state_happensbefore: String = "state_happensBefore"

  private val state_callTransaction: String = "state_callTransaction"

  private val state_currenttransaction: String = "state_currentTransaction"

  private val state_currenttransactionId: String = "state_currentTransactionId"

  private val state_origin: String = "state_origin"

  private val state_invocations: String = "state_invocations"

  private val state_invocationResult: String = "state_" + invocationResult

  private val state_invocationHappensBefore: String = "state_invocationHappensBefore"


  // TODO needs to be done per type
  private def state_knownIds(t: String): String = s"state_knownIds_$t"

  private def state_exposed(t: String): String = s"state_exposed_$t"

  private def state_locallyGenerated(t: String): String = s"state_locallyGenerated_${typeName(t)}"

  private val CallId: String = "CallId"

  private val InvocationId: String = "InvocationId"

  private val TransactionId: String = "TransactionId"

  private val noInvocation: String = "NoInvocation"

  private val NoResult: String = "NoResult"

  private val noop: String = "Noop"


  def MapType(keyType: TypeExpression, resultType: TypeExpression): TypeExpression = {
    MapType(List(keyType), resultType)
  }

  def MapType(keyTypes: List[TypeExpression], resultType: TypeExpression): TypeExpression = {
    val keyType: TypeExpression = keyTypes match {
      case List(t: TypeExpression) =>
        t
      case _ =>
        TupleType(keyTypes)
    }
    TypeSymbol(LQualid(List("Map"), "map"), List(keyType, resultType))
  }

  def ref(t: TypeExpression): TypeExpression = {
    TypeSymbol("ref", List(t))
  }

  def TypeBool(): TypeExpression = {
    TypeSymbol("bool")
  }

  def TypeInt(): TypeExpression = {
    TypeSymbol("int")
  }

  def transformOp(o: CrdtTypeDefinition.Operation): InOperationDecl = {
    var pList = List[InVariable]()
    var count = 0
    for (oType <- o.paramTypes) {
      pList = pList :+ InVariable(
        source = NoSource(),
        name = Identifier(NoSource(), "args" + count),
        typ = oType
      )
      count += 1
    }
    InOperationDecl(
      source = NoSource(),
      name = Identifier(NoSource(), o.name),
      params = pList
    )
  }

  //  def addCrdtOperations(prog: InProgram): InProgram = {
  //    val crdt = prog.programCrdt
  //
  //    prog.copy(
  //      queries = prog.queries ++ crdt.queryDefinitions(),
  //      operations = prog.operations ++ crdt.operations.map(transformOp)
  //    )
  //  }

  def transformProgram(programContext: InProgram): Module = {

    procedures = programContext.procedures
    procedureNames = procedures.map(_.name.name).toSet

    stateVars = List(
      GlobalVariable(state_callops, ref(MapType(List(typeCallId), typeOperation))),
      GlobalVariable(state_visiblecalls, ref(MapType(List(typeCallId), TypeBool()))),
      GlobalVariable(state_happensbefore, ref(MapType(typeCallId, MapType(typeCallId, TypeBool())))),
      GlobalVariable(state_callTransaction, ref(MapType(typeCallId, typeTransactionId))),
      GlobalVariable(state_currenttransaction, ref(MapType(List(typeCallId), TypeBool()))),
      GlobalVariable(state_currenttransactionId, ref(typeTransactionId)),
      GlobalVariable(state_origin, ref(MapType(List(typeCallId), typeInvocationId))),
      GlobalVariable(state_invocations, ref(MapType(List(typeInvocationId), typeInvocationInfo))),
      GlobalVariable(state_invocationResult, ref(MapType(List(typeInvocationId), typeInvocationResult))),
      GlobalVariable(state_invocationHappensBefore, ref(MapType(List(typeInvocationId, typeInvocationId), TypeBool())))
    )


    // generate types
    generateUserDefinedTypes(programContext)
    generateDerivedTypes()


    // generate operations

    val operationCases = for (opDecl <- programContext.programCrdt.operations()) yield {
      val name = opDecl.name
      val paramTypes: List[TypedParam] = opDecl.params.map(transformParamToTypeParam)

      functionReplacements += (name -> operationCaseName(name))
      operationDefs += (name -> paramTypes)

      TypeCase(
        name = operationCaseName(name),
        paramsTypes = paramTypes
      )
    }

    val operationType = TypeDecl(
      name = operation,
      definition = AlgebraicType(
        List(TypeCase(
          name = noop,
          paramsTypes = List()
        )) ++ operationCases
      )
    )
    types += (operation -> operationType)


    implicit val ctxt = Context()

    // add custom query functions
    for (query <- programContext.programCrdt.queryDefinitions()) {
      val name = query.name.name
      val impl = query.implementation.map(transformExpr)
      val ensures = query.ensures.map(transformExpr) // TODO encode postcondition somewhere ... maybe add axioms?

      val params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ))
      queryFunctions += (name -> LogicDecls(List(
        LogicDecl(
          name = name,
          params = params,
          returnType = transformTypeExpr(query.returnType),
          implementation = impl
        )
      )))
      queryProcedures :+= AbstractFunction(
        name = s"${name}_proc",
        params = params,
        returnType = transformTypeExpr(query.returnType),
        specs = List(
          Ensures("result" === FunctionCall(name, params.map(p => Symbol(p.name))))
        )
      )
      //      val specs = query.implementation match {
      //        case Some(impl) =>
      //          List(
      //            Ensures("result" === transformExpr(impl))
      //          )
      //        case None =>
      //          List()
      //        // TODO handle other functions
      //      }
      //      queryFunctions += (name ->
      //        AbstractFunction(
      //          name = name,
      //          params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ)),
      //          returnType = transformTypeExpr(query.returnType),
      //          specs = specs
      //        )
      //        )

      //        GlobalLet(
      //        name = name,
      //        funBody = FunBody(
      //          params = query.params.toList.map(transformVariableToTypeParam) ++ stateVars.map(g => TypedParam(g.name, g.typ)),
      //          returnType = Some(transformTypeExpr(query.returnType)),
      //          body = query.implementation.map(transformExpr).get // TODO handle functions without body
      //        )
      //        arguments =
      //        resultType = transformTypeExpr(query.returnType),
      //        implementation = query.implementation.map(transformExpr),
      //        attributes = if (query.annotations.contains(InlineAnnotation())) List(Attribute("inline")) else List()
      //      ))
    }


    // add invariants
    invariants = for (inv <- programContext.invariants) yield {
      transformExpr(inv.expr).setTrace(AstElementTraceInfo(inv))
    }

    val standardProcedures: List[MDecl] = List(
      makeProcBeginAtomic(),
      makeProcEndAtomic(),
      makeProcCrdtOperation(),
      makeStartInvocationProcedure(),
      makeFinishInvocationProcedure()
    )

    val translatedProcedures: List[GlobalLet] = for (procedure <- procedures) yield {
      transformProcedure(procedure)
    }

    val axioms = for (axiom <- programContext.axioms) yield {
      Axiom(
        name = lIdent("someAxiom"),
        formula = Forall(stateVars.map(g => TypedParam(g.name, g.typ)), transformExpr(axiom.expr)))
    }

    val imports = List(
      Import(false, ImpExpImport(), TQualid(List[LIdent]("map"), "Map")),
      Import(false, ImpExpImport(), TQualid(List[LIdent]("ref"), "Ref")),
      Import(false, ImpExpImport(), TQualid(List[LIdent]("int"), "Int"))
    )

    Module(
      name = "CrdtProgram",
      labels = List(),
      declarations = List()
        ++ imports
        ++ types.values.map(d => TypeDecls(List(d))) // List(TypeDecls(types.values.toList))
        ++ stateVars
        ++ newIdTypes.map(containsIdFunc(_, programContext.programCrdt.operations()))
        ++ queryFunctions.values
        ++ queryProcedures
        ++ axioms
        ++ List(makeFunc_WellFormed())
        ++ standardProcedures
        ++ List(initialStateProc())
        //        ++ List(mergeStateProc())
        ++ translatedProcedures
    )
    //    Program(List()
    //      ++ types.values
    //      ++ stateVars
    //      ++ queryFunctions.values
    //      ++ axioms
    //      ++ List(makeFunc_WellFormed())
    //      ++ standardProcedures
    //      ++ List(initialStateProc())
    //      ++ translatedProcedures)
  }


  def operationCaseName(name: String): String = {
    "Op_" + name
  }

  def typeName(name: String): String = s"${name.charAt(0).toLower}${name.substring(1)}"

  def constructorName(name: String): String = s"${name.charAt(0).toUpper}${name.substring(1)}"

  def generateUserDefinedTypes(programContext: InProgram): Unit = {
    // user defined data types:
    for (typeDecl <- programContext.types) {
      val name: String = typeName(typeDecl.name.name)


      if (typeDecl.dataTypeCases.isEmpty) {
        val typeDef =
          restrictDomains match {
            case None => AbstractType()
            case Some(n) =>
              val cases = for (i <- (1 to n)) yield TypeCase(
                name = constructorName(name + "_" + i)
              )
              AlgebraicType(
                cases = cases.toList
              )
          }
        val t = TypeDecl(
          name = name,
          definition = typeDef
        )
        types += (name -> t)

        if (typeDecl.isIdType) {
          // for id types create additional helpers:

          // set of known IDs
          stateVars +:= GlobalVariable(state_knownIds(name), ref(MapType(List(TypeSymbol(name)), TypeBool())))
          // exposed(uid, callId) -> bool
          // states that the given uid was part of the arguments in the given callId
          stateVars +:= GlobalVariable(state_exposed(name), ref(MapType(List(TypeSymbol(name), typeCallId), TypeBool())))
          // set of locally generated ids
          stateVars +:= GlobalVariable(state_locallyGenerated(name), ref(MapType(List(TypeSymbol(name)), TypeBool())))


          // containsId function for operations:
          newIdTypes +:= name
        }
      } else {
        // Datatype
        val dtcases = for (dtCase <- typeDecl.dataTypeCases) yield {
          val name = dtCase.name.name.capitalize
          functionReplacements += (dtCase.name.name -> name)
          TypeCase(
            name = name,
            paramsTypes = dtCase.params.toList.map(transformVariableToTypeParam)
          )
        }
        val t = TypeDecl(
          name = name,
          definition = AlgebraicType(dtcases)
        )
        types += (name -> t)

      }
    }
  }


  def containsId(idType: String): String = s"containsId_$idType"

  def containsIdFunc(idType: String, operations: List[Operation]): Declaration = {

    // go through all cases and check if id is contained
    val cases = for (opDecl <- operations) yield {
      val name = opDecl.name


      var check: Term = WhyAst.BoolConst(false)

      for (arg <- opDecl.params) {
        arg.typ match {
          case t: IdType =>
            if (typeName(t.name) == idType) {
              check = check || (arg.name === "idT")
            }
          case _ =>
          // TODO handle nested types like datatypes
        }
      }

      TermCase(
        pattern = ConstructorPattern(
          constructorName = operationCaseName(name),
          args = opDecl.params.map(arg => VariablePattern(arg.name))
        ),
        term = check
      )
    }
    //    GlobalLet(
    //      name = containsId(idType),
    //      funBody = FunBody(
    //        params = List(
    //          TypedParam("op", typeOperation),
    //          TypedParam("idT", TypeSymbol(idType))
    //        ),
    //        returnType = Some(TypeBool()),
    //        body = MatchTerm(
    //          terms = List("op"),
    //          cases = cases
    //            ++ List(
    //            TermCase(ConstructorPattern(noop, List()), BoolConst(false))
    //          )
    //        )
    //      )
    //    )
    LogicDecls(
      List(
        LogicDecl(
          name = containsId(idType),
          params = List(
            TypedParam("op", typeOperation),
            TypedParam("idT", TypeSymbol(idType))
          ),
          returnType = TypeBool(),
          implementation = Some(
            MatchTerm(
              terms = List("op"),
              cases = cases
                ++ List(
                TermCase(ConstructorPattern(noop, List()), WhyAst.BoolConst(false))
              )
            )
          )
        )
      )
    )
  }

  def generateCallIdType(): Unit = {
    val cases: List[TypeCase] =
      restrictCalls match {
        case None =>
          List(
            TypeCase(
              name = CallId,
              paramsTypes = List(TypedParam("id", TypeInt()))
            )
          )
        case Some(n) =>
          for (i <- (1 to n).toList) yield TypeCase(
            name = CallId + i
          )
      }
    val callIdType = TypeDecl(
      name = callId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = cases
      )
    )
    types += (callId -> callIdType)
  }

  def generateInvocationIdType(): Unit = {
    val cases: List[TypeCase] =
      restrictInvocations match {
        case None =>
          List(
            TypeCase(
              name = InvocationId,
              paramsTypes = List(TypedParam("id", TypeInt()))
            )
          )
        case Some(n) =>
          for (i <- (1 to n).toList) yield TypeCase(
            name = InvocationId + i
          )
      }
    val invocationIdType = TypeDecl(
      name = invocationId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = cases
      )
    )
    types += (invocationId -> invocationIdType)
  }

  def generateTransactionIdType(): Unit = {
    val cases: List[TypeCase] =
      List(
        TypeCase(
          name = TransactionId,
          paramsTypes = List(TypedParam("id", TypeInt()))
        )
      )
    val transactionIdType = TypeDecl(
      name = transactionId,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = cases
      )
    )
    types += (transactionId -> transactionIdType)
  }

  def generateDerivedTypes(): Unit = {
    // callId type
    generateCallIdType()

    // invocationId type
    generateInvocationIdType()

    // transactionId type
    generateTransactionIdType()

    // invocationInfo type
    val invocationInfoCases = for (procedure <- procedures) yield {
      TypeCase(
        name = invocationInfoForProc(procedure.name.name),
        paramsTypes = procedure.params.map(transformVariableToTypeParam)
      )
    }

    val invocationInfoType = TypeDecl(
      name = invocationInfo,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List( // TODO add cases for other procedures
          TypeCase(
            name = noInvocation,
            paramsTypes = List()
          )
        ) ++ invocationInfoCases
      )
    )

    types += (invocationInfo -> invocationInfoType)

    // invocationResult type
    val invocationResultCases = for (procedure <- procedures) yield {
      val procName: String = procedure.name.name
      val name = invocationInfoForProc(procName)
      val args: List[TypedParam] = procedure.params.map(transformVariableToTypeParam)

      functionReplacements += (s"${procName}_res" -> invocationResForProc(procName))

      TypeCase(
        name = invocationResForProc(procName),
        paramsTypes = procedure.returnType match {
          case UnitType() =>
            List()
          case rt =>
            List(TypedParam("result", transformTypeExpr(rt)))
        }
      )
    }

    val invocationResultType = TypeDecl(
      name = invocationResult,
      typeParameters = List(),
      definition = AlgebraicType(
        cases = List( // TODO add cases for other procedures
          TypeCase(
            name = NoResult,
            paramsTypes = List()
          )
        ) ++ invocationResultCases
      )
    )
    types += (invocationResult -> invocationResultType)
  }

  def invocationResForProc(procName: String): String = {
    s"${procName.capitalize}_res"
  }


  def invocationInfoForProc(procName: String): String = {
    "Invocation_" + procName
  }

  //  def sortTypes(types: Iterable[TypeDecl], constructors: List[FuncDecl]): List[Declaration] = {
  //    var result = List[Declaration]()
  //
  //    for (t <- types) {
  //      result = result ++ List(t) ++ (for (constr <- constructors; if constr.resultType == TypeSymbol(t.name)) yield constr)
  //    }
  //
  //    result
  //  }


  val check_initialState: String = "check_initialState"

  /**
   * a procedure to check if the initial state satisfies all invariants
   */
  def initialStateProc(): GlobalLet = {
    GlobalLet(
      isGhost = false,
      name = check_initialState,
      labels = List(),
      funBody = FunBody(
        params = List(),
        returnType = Some(unitType()),
        specs = List(
          Requires(
            Forall("c" :: typeCallId, state_callops.get("c") === noop.call())),
          Requires(
            Forall("c" :: typeCallId, !state_visiblecalls.get("c"))),
          Requires(
            Forall(List("c1" :: typeCallId, "c2" :: typeCallId), !happensBefore("c1", "c2"))),
          //          Requires(
          //            Forall(List("c1" :: typeCallId, "c2" :: typeCallId), state_callTransaction.get("c1") !== state_callTransaction.get("c2"))),
          Requires(
            Forall("c" :: typeCallId, !state_currenttransaction.get("c"))),
          Requires(
            Forall("i" :: typeInvocationId, state_invocations.get("i") === noInvocation.call())),
          Requires(
            Forall("i" :: typeInvocationId, state_invocationResult.get("i") === NoResult.call())),
          Requires(
            Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
              !state_invocationHappensBefore.get("i1", "i2"))))
          ++ wellformedConditions().map(Ensures(_))
          ++ invariants.map(inv => Ensures(inv)),
        otherSpecs = List(),
        body = Tuple(List())
      )
    )
  }

  private def happensBefore(c1: Term, c2: Term) = {
    state_happensbefore.get(c2).getF(c1)
  }


  /**
   * adds the given postfix to all occurrences of a state-variable
   */
  def postfixStateVars(term: Term, postfix: String): Term = {

    def visitSpec(s: Spec): Spec = s match {
      case Requires(formula) => Requires(visit(formula))
      case Ensures(formula) => Ensures(visit(formula))
      case Returns(cases) => Returns(cases.map(visitFormulaCase))
      case Reads(terms) => Reads(terms.map(visit))
      case Writes(terms) => Writes(terms.map(visit))
      case RaisesName(raised) => RaisesName(raised)
      case Raises(cases) => Raises(cases.map(visitRaiseCase))
      case Variant(variants) => Variant(variants.map(visitOneVariant))
    }

    def visitVariant(s: Variant): Variant = Variant(s.variants.map(visitOneVariant))

    def visitFormulaCase(f: FormulaCase): FormulaCase =
      f.copy(formula = visit(f.formula))

    def visitRaiseCase(r: RaisesCase): RaisesCase =
      r.copy(formula = visit(r.formula))

    def visitOneVariant(v: OneVariant): OneVariant =
      v.copy(term = visit(v.term))

    def visitInv(i: Invariant): Invariant =
      Invariant(visit(i.formula))

    def visitCase(c: TermCase): TermCase =
      TermCase(c.pattern, visit(c.term))

    def visitTermField(t: TermField): TermField =
      TermField(t.fieldName, visit(t.term))

    def visit(t: Term): Term = t match {
      case WhyAst.IntConst(value) => t
      case RealConstant(value) => t
      case WhyAst.BoolConst(value) => t
      case Symbol(name) =>
        if (stateVars.exists(v => v.name.toString == name.toString)) {
          Symbol(name.toString + postfix)
        } else {
          t
        }
      case FunctionCall(funcName, args) =>
        FunctionCall(funcName, args.map(visit))
      case ArrayLookup(arrayTerm, indexTerm) =>
        ArrayLookup(visit(arrayTerm), visit(indexTerm))
      case ArrayUpdate(arrayTerm, indexTerm, newValue) =>
        ArrayUpdate(visit(arrayTerm), visit(indexTerm), visit(newValue))
      case Conditional(condition, ifTrue, ifFalse) =>
        Conditional(visit(condition), visit(ifTrue), visit(ifFalse))
      case LambdaAbstraction(params, specs, otherSpecs, body) =>
        LambdaAbstraction(params, specs.map(visitSpec), otherSpecs.map(visitSpec), visit(body))
      case LetTerm(pattern, value, body) =>
        LetTerm(pattern, visit(value), visit(body))
      case Sequence(terms) =>
        Sequence(terms.map(visit))

      case Loop(invs, variant, body) =>
        Loop(invs.map(visitInv), variant.map(visitVariant), visit(body))
      case While(condition, invs, variant, body) =>
        While(visit(condition), invs.map(visitInv), variant.map(visitVariant), visit(body))
      case AnyTerm(typ, specs) =>
        AnyTerm(typ, specs.map(visit))
      case MatchTerm(terms, cases) =>
        MatchTerm(terms.map(visit), cases.map(visitCase))
      case QuantifierTerm(quantifier, binders, body) =>
        QuantifierTerm(quantifier, binders, visit(body))
      case Tuple(values) =>
        Tuple(values.map(visit))
      case RecordTerm(fields) =>
        RecordTerm(fields.map(visitTermField))
      case FieldAccess(recordTerm, fieldName) =>
        FieldAccess(visit(recordTerm), fieldName)
      case FieldAssignment(recordTerm, fieldName, newValue) =>
        FieldAssignment(visit(recordTerm), fieldName, visit(newValue))
      case FieldUpdate(recordTerm, fieldUpdates) =>
        FieldUpdate(visit(recordTerm), fieldUpdates.map(visitTermField))
      case CastTerm(ter, typ) =>
        CastTerm(visit(ter), typ)
      case LabeledTerm(label, ter) =>
        LabeledTerm(label, visit(ter))
      case CodeMark(name) =>
        CodeMark(name)
      case Old(ter) =>
        Old(visit(ter))
      case Assert(formula) =>
        Assert(visit(formula))
      case Assume(formula) =>
        Assume(visit(formula))
      case Check(formula) =>
        Check(visit(formula))
    }

    return visit(term)
  }


  val beginAtomic: String = "beginAtomic"

  val wellFormed: String = "wellFormed"

  def generatedIdAssumptions(): List[Ensures] = {
    for (idType <- newIdTypes) yield {
      val t = TypeSymbol(idType)

      // for every call containing a locally generated id
      // we can find a call from the current invocation containing the id and happening before
      Ensures(
        Forall(List("c" :: typeCallId, "id" :: t),
          (containsId(idType).call(state_callops.get("c"), "id")
            && state_locallyGenerated(idType).get("id"))
            ==> Exists("lc" :: typeCallId,
            containsId(idType).call(state_callops.get("lc"), "id")
              && happensBefore("lc", "c")
              && state_origin.get("lc") === "currentInvocation"
          )
        )
      )
    }
  }

  /**
   * procedure to start a transaction
   */
  def makeProcBeginAtomic(): AbstractFunction = {

    // beginAtomic can change all state-vars
    val writes: List[Symbol] = List(
      state_callops,
      state_visiblecalls,
      state_happensbefore,
      state_callTransaction,
      state_currenttransaction,
      state_origin,
      state_invocations,
      state_invocationResult,
      state_invocationHappensBefore,
      state_currenttransactionId
    )
    builtinFuncWrites += (beginAtomic -> writes)

    AbstractFunction(
      name = beginAtomic,
      params = List(currentInvocation :: typeInvocationId),
      returnType = unitType(),
      specs = List(
        Writes(writes),
        // new transaction has no calls yet:
        Ensures(Forall("c" :: typeCallId, !state_currenttransaction.get("c"))),
        // well formed history:
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => Symbol(g.name))))
      )
        // invariant maintained:
        ++ invariants.map(Ensures)
        ++ List(
        // causally consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c2") && happensBefore("c1", "c2"))
              ==> state_visiblecalls.get("c1"))),
        // transaction consistent:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
            (state_visiblecalls.get("c1")
              && (state_callops.get("c2") !== noop.call())
              && sameTransaction("c1", "c2"))
              ==> state_visiblecalls.get("c2"))),
        // monotonic growth of visiblecalls
        Ensures(
          Forall("c" :: typeCallId, Old(state_visiblecalls).get("c")
            ==> state_visiblecalls.get("c"))),
        // monotonic growth of callops
        Ensures(
          Forall("c" :: typeCallId, (Old(state_callops).get("c") !== noop.call())
            ==> (state_callops.get("c") === Old(state_callops).get("c")))),
        // monotonic growth of happensbefore
        // --> no new calls can be added before:
        Ensures(
          Forall(List("c1" :: typeCallId, "c2" :: typeCallId), (Old(state_callops).get("c2") !== noop.call())
            ==> (happensBefore("c1", "c2") === Old(state_happensbefore).get("c2").getF("c1")))),
        // monotonic growth of sameTransaction
        Ensures(
          Forall(List("c" :: typeCallId), (Old(state_callops).get("c") !== noop.call())
            ==> (state_callTransaction.get("c") === Old(state_callTransaction).get("c")))),
        // monotonic growth of origin
        Ensures(
          Forall("c" :: typeCallId, (Old(state_callops).get("c") !== noop.call())
            ==> (state_origin.get("c") === Old(state_origin).get("c")))),
        // monotonic growth of invocations
        Ensures(
          Forall("i" :: typeInvocationId, (Old(state_invocations).get("i") !== noInvocation.call())
            ==> (state_invocations.get("i") === Old(state_invocations).get("i")))),
        // monotonic growth of invocationResult
        Ensures(
          Forall("i" :: typeInvocationId, (Old(state_invocationResult).get("i") !== NoResult.call())
            ==> (state_invocationResult.get("i") === Old(state_invocationResult).get("i")))),
        // monotonic growth of invocationHappensBefore
        // --> no new calls can be added before:
        Ensures(
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId), (Old(state_invocationResult).get("i2") !== NoResult.call())
            ==> (state_invocationHappensBefore.get("i1", "i2") === Old(state_invocationHappensBefore).get("i1", "i2"))))
      ) ++ generatedIdAssumptions()
    )
  }

  val endAtomic: String = "endAtomic"

  /**
   * procedure to end a transaction.
   * at the end of a transaction we check the invariants
   */
  def makeProcEndAtomic(): AbstractFunction = {

    // TODO should add operations from current transaction?

    // TODO should check invariant after endAtomic?

    builtinFuncWrites += (endAtomic -> List())

    AbstractFunction(
      name = endAtomic,
      params = List(),
      returnType = unitType(),
      specs =
        invariants.map(Requires)
    )
  }


  val crdtOperation: String = "crdtOperation"

  val currentInvocation = "currentInvocation"

  /**
   * a procedure to execute a CRDT operation
   */
  def makeProcCrdtOperation(): AbstractFunction = {


    val newCallId: Expr = "result"

    val writes: List[Symbol] = List(state_callops, state_happensbefore, state_visiblecalls, state_currenttransaction, state_origin)
    builtinFuncWrites += (crdtOperation -> writes)

    AbstractFunction(
      name = crdtOperation,
      params = List(currentInvocation :: typeInvocationId, operation :: typeOperation),
      returnType = typeCallId,
      specs = List(
        Writes(writes),
        Ensures(
          Old(state_callops.get(newCallId)) === (noop.call())),
        Ensures(state_callops.deref() === Old(state_callops).update(newCallId, operation)),
        //        Ensures(
        //          Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
        //            happensBefore("c1", "c2")
        //              <==> (Old(state_happensbefore).get("c2").get("c1")
        //              || ((state_visiblecalls.get("c1") || "c1" === "c2") && "c2" === newCallId)))),
        Ensures(
          state_happensbefore.deref() === Old(state_happensbefore).update(newCallId, state_visiblecalls.deref())

        ),
        Ensures(
          state_visiblecalls.deref() === Old(state_visiblecalls).update(newCallId, WhyAst.BoolConst(true))),

        // TODO update current transaction and sameTransaction
        // current transaction update:
        Ensures(
          state_currenttransaction.deref() === Old(state_currenttransaction).update(newCallId, WhyAst.BoolConst(true))),
        Ensures(
          state_callTransaction.get(newCallId) === state_currenttransactionId.deref()
        ),
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        // update state_origin
        Ensures(
          state_origin.deref() === Old(state_origin).update(newCallId, "currentInvocation"))
      )
    )
  }


  val startInvocation: String = "startInvocation"

  val newInvocId: String = "newInvocId"

  val result: Term = "result"

  /**
   * a procedure used at the start of each invocation to setup the local state etc.
   */
  def makeStartInvocationProcedure(): AbstractFunction = {

    val writes: List[Symbol] = List(state_invocations)
    builtinFuncWrites += (startInvocation -> writes)

    val noLocallyGenerated: List[Ensures] = for (idType <- newIdTypes) yield {
      val t: TypeExpression = TypeSymbol(idType)
      Ensures(Forall("id" :: t, !state_locallyGenerated(idType).get("id")))
    }

    AbstractFunction(
      name = startInvocation,
      params = List("invocation" :: typeInvocationInfo),
      returnType = typeInvocationId,
      specs = List(
        Writes(writes),
        // one fresh invocation added:
        Ensures(
          Old(state_invocations).get(result) === noInvocation.call()),
        Ensures(
          state_invocationResult.get(result) === NoResult.call()),
        Ensures(
          state_invocations.get(result) === "invocation"),
        // other invocations unchanged:
        Ensures(
          state_invocations.deref() === Old(state_invocations).update(result, "invocation"))
        // new invocation not in hb (TODO move to wellformed)
        //        Ensures(
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("i", "newInvocId")))),
        //        Ensures(
        //          Forall("i" :: typeInvocationId, Old(!"state_invocationHappensBefore".get("newInvocId", "i")))),
        // current invocation: happensBefore
        //        Ensures(
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
      ) ++ noLocallyGenerated
    )
  }

  val finishInvocation: String = "finishInvocation"

  /**
   * a procedure used at the end of each invocation
   */
  def makeFinishInvocationProcedure(): AbstractFunction = {

    val writes: List[Symbol] = List(state_invocationResult, state_invocationHappensBefore)
    builtinFuncWrites += (finishInvocation -> writes)

    AbstractFunction(
      name = finishInvocation,
      params = List(newInvocId :: typeInvocationId, "res" :: typeInvocationResult),
      returnType = unitType(),
      specs = List(
        Writes(writes),
        Ensures(
          FunctionCall(wellFormed, stateVars.map(g => IdentifierExpr(g.name)))),
        //        // origin for new calls:
        //        Ensures(
        //          Forall("c" :: typeCallId, Old("state_inCurrentInvocation".get("c")) ==> ("state_origin".get("c") === "newInvocId"))),
        //        // old calls unchanged:
        //        Ensures(
        //          Forall("c" :: typeCallId, (!Old("state_inCurrentInvocation".get("c"))) ==> ("state_origin".get("c") === Old("state_origin".get("c"))))),
        // one fresh invocation added:
        //        Ensures(
        //          Old("state_invocations".get("newInvocId") === "NoInvocation".$())),
        Ensures(
          state_invocationResult.get(newInvocId) === "res"),
        // other invocations unchanged:
        Ensures(
          state_invocationResult.deref() === Old(state_invocationResult).update(newInvocId, "res")),
        // new invocation not in hb before the call (TODO move to wellformed)
        Ensures(
          Forall("i" :: typeInvocationId, !Old(state_invocationHappensBefore).get("i", newInvocId))),
        Ensures(
          Forall("i" :: typeInvocationId, !Old(state_invocationHappensBefore).get(newInvocId, "i"))),
        // current invocation calls cleared
        // current invocation: happensBefore
        //        Ensures(
        //          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        //            "state_invocationHappensBefore".get("i1", "i2") === (
        //              // either already in old hb
        //              Old("state_invocationHappensBefore".get("i1", "i2")))))
        // helper: calls from invocations that happened before, also happen before the current one
        Ensures(
          Forall(List("i" :: typeInvocationId, "c1" :: typeCallId, "c2" :: typeCallId),
            (state_invocationHappensBefore.get("i", newInvocId)
              && Old(state_origin).get("c1") === "i"
              && Old(state_origin).get("c2") === newInvocId)
              ==> happensBefore("c1", "c2")
          )
        ),
        // TODO real version:
        Ensures(
          Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
            state_invocationHappensBefore.get("i1", "i2") === (
              // either already in old hb
              Old(state_invocationHappensBefore).get("i1", "i2")
                // or part of the new hb
                || (("i2" === newInvocId)
                && Exists("c" :: typeCallId, Old(state_origin).get("c") === newInvocId)
                && Exists("c" :: typeCallId, state_origin.get("c") === "i1")
                && Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
                ((state_origin.get("c1") === "i1") && Old(state_origin).get("c2") === newInvocId) ==> happensBefore("c1", "c2"))))))
      )
    )
  }

  /**
   * a function that takes all state vars and checks whether the state is well-formed
   */
  def makeFunc_WellFormed(): LogicDecls = {
    val i: Expr = "i"
    val body = wellformedConditions().reduce(_ && _)
    LogicDecls(List(
      LogicDecl(
        name = wellFormed,
        params = stateVars.map(g => g.name :: g.typ),
        returnType = TypeBool(),
        implementation = Some(body)
      )
    ))
  }

  /**
   * the conditions required to check well-formedness
   */
  def wellformedConditions(): List[Term] = {
    val i: Expr = "i"
    List(
      // no happensBefore relation between non-existing calls
      "happensBefore_exists_l" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_callops.get("c1") === (noop.call()))
            ==> !happensBefore("c1", "c2")
        ),
      "happensBefore_exists_r" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_callops.get("c2") === (noop.call()))
            ==> !happensBefore("c1", "c2")
        ),
      // visible calls are a subset of all calls
      "visibleCalls_exist" %%:
        Forall("c" :: typeCallId, state_visiblecalls.get("c") ==> (state_callops.get("c") !== (noop.call()))),
      // visible calls forms consistent snapshot
      "visibleCalls_transaction_consistent1" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_visiblecalls.get("c1") && sameTransaction("c1", "c2") && (state_callops.get("c2") !== (noop.call()))) ==> state_visiblecalls.get("c2")),
      //      "visibleCalls_transaction_consistent2" %%:
      //        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
      //          (state_visiblecalls.get("c2") && sameTransaction("c1", "c2")) ==> state_visiblecalls.get("c1")),
      "visibleCalls_causally_consistent" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          (state_visiblecalls.get("c2") && happensBefore("c1", "c2")) ==> state_visiblecalls.get("c1")),

      // happensBefore is a partial order (reflexivity, transitivity, antisymmetric)
      "happensBefore_reflex" %%:
        Forall("c" :: typeCallId, (state_callops.get("c") !== (noop.call())) ==> happensBefore("c", "c")),
      "happensBefore_trans" %%:
        Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
          (happensBefore("x", "y") && happensBefore("y", "z")) ==> happensBefore("x", "z")),
      "happensBefore_antisym" %%:
        Forall(List("x" :: typeCallId, "y" :: typeCallId), (happensBefore("x", "y") && happensBefore("y", "x")) ==> ("x" === "y")),
      // invocation happens-before of origins implies happens-before of calls
      "happensBefore_reflex" %%:
        Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
          ((state_callops.get("c1") !== noop.call())
            && (state_callops.get("c2") !== noop.call())
            && state_invocationHappensBefore.get(state_origin.get("c1"), state_origin.get("c2")))
            ==> happensBefore("c1", "c2")),
      // no invocation implies no result
      Forall("i" :: typeInvocationId,
        (state_invocations.get("i") === noInvocation.call()) ==> (state_invocationResult.get("i") === NoResult.call())),
      // no result implies not in invocation happens before
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        (state_invocationResult.get("i1") === NoResult.call()) ==> !state_invocationHappensBefore.get("i1", "i2")),
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        (state_invocationResult.get("i1") === NoResult.call()) ==> !state_invocationHappensBefore.get("i2", "i1")),
      // in happens before implies not NoResult
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        state_invocationHappensBefore.get("i1", "i2") ==> (state_invocationResult.get("i1") !== NoResult.call())),
      Forall(List("i1" :: typeInvocationId, "i2" :: typeInvocationId),
        state_invocationHappensBefore.get("i1", "i2") ==> (state_invocationResult.get("i2") !== NoResult.call())),
      // no sameTransaction relation between non-existing calls
      //      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
      //        (state_callops.get("c1") === (noop $()))
      //          ==> !sameTransaction("c1","c2")
      //      ),
      //      Forall(List("c1" :: typeCallId, "c2" :: typeCallId),
      //        (state_callops.get("c2") === (noop $()))
      //          ==> !sameTransaction("c1","c2")
      //      ),
      // sameTransaction is a equivalence relation (reflexive, transitive, symmetric)
      //      Forall("c" :: typeCallId, (state_callops.get("c") !== (noop $())) ==> sameTransaction("c","c")),
      //      Forall(List("x" :: typeCallId, "y" :: typeCallId, "z" :: typeCallId),
      //        (sameTransaction("x","y") && sameTransaction("y","z")) ==> sameTransaction("x","z")),
      //      Forall(List("x" :: typeCallId, "y" :: typeCallId), sameTransaction("x","y") === sameTransaction("y","x")),
      // transaction consistency with happens before:
      Forall(List("x1" :: typeCallId, "x2" :: typeCallId, "y1" :: typeCallId, "y2" :: typeCallId),
        (sameTransaction("x1", "x2")
          && sameTransaction("y1", "y2")
          && !sameTransaction("x1", "y1")
          && happensBefore("x1", "y1"))
          ==> happensBefore("x2", "y2"))

    )
  }

  def sameTransaction(c1: Term, c2: Term): Term =
    state_callTransaction.get(c1) === state_callTransaction.get(c2)

  //  def transformLocals(body: StmtContext): Term = {
  //    var locals = List[Term]()
  //    val listener = new LangBaseVisitor[Unit] {
  //      override def visitLocalVar(lv: LangParser.LocalVarContext): Unit = {
  //        locals +:= transformLocalVar(lv)
  //      }
  //    }
  //    body.accept(listener)
  //    makeBlock(locals)
  //  }

  /**
   * returns the default value for the given type
   *
   * (used to init refs)
   */
  def defaultValue(typ: InTypeExpr): Term = {
    typ match {
      case BoolType() =>
        return WhyAst.BoolConst(false)
      case IntType() =>
        return WhyAst.IntConst(0)
      case AnyType()
           | CallIdType()
           | InvocationIdType()
           | InvocationInfoType()
           | InvocationResultType()
           | SomeOperationType()
           | OperationType(_)
           | FunctionType(_, _, _)
           | SimpleType(_, _)
           | IdType(_)
           | _: TransactionIdType =>
        AnyTerm(transformTypeExpr(typ))
    }
  }


  /**
   * create let-constructs for local variables around body
   */
  def transformLocals(vars: List[InVariable])(body: Term): Term = {
    vars.foldRight(body)((v, b) => {
      LetTerm(
        pattern = VariablePattern(v.name.name),
        value = FunctionCall("ref", List(defaultValue(v.typ))),
        body = b
      )
    })
  }


  def assignedVars(term: Term): Set[String] = {
    var result = Set[String]()
    walk(term) {
      case FunctionCall(LQualid(List(), LIdent(":=")), List(Symbol(left), _right)) =>
        result += left.toString
      case FunctionCall(LQualid(List(), LIdent(m)), _args) if builtinFuncWrites.contains(m) =>
        result ++= builtinFuncWrites(m).map(_.name.toString)
    }
    result
  }

  /**
   * Transforms a procedure into a why-function with the
   * pre- and post-conditions that need to be checked
   */
  def transformProcedure(procedure: InProcedure): GlobalLet = {


    val procname: String = procedure.name.name
    val params: List[TypedParam] = procedure.params.map(transformVariable)
    val paramNames: List[Symbol] = params.map(p => IdentifierExpr(p.name))
    val specContext: Context = Context(
      procedureName = procname,
      procedureArgNames = paramNames,
      refVars = procedure.locals.map(_.name.name).toSet
    )

    val bodyCtxt = specContext.copy(targetIsLogic = false)


    val body = LetTerm(
      pattern = VariablePattern("new" + InvocationId),
      value = FunctionCall(startInvocation, List(FunctionCall(invocationInfoForProc(procname), paramNames))),
      body = makeBlock(
        // call endAtomic to check invariants (TODO make extra procedure to check invariants)
        FunctionCall(endAtomic, List()),
        // execute procedure body:
        transformStatement(procedure.body)(bodyCtxt),
        if (procedure.returnType == UnitType()) {
          makeReturn(None, List(), procedure)(bodyCtxt)
        } else {
          makeBlock()
        },
        Tuple(List())
      )
    )

    val bodyWithLocals = transformLocals(procedure.locals)(body)

    val writes = assignedVars(bodyWithLocals)
      .filter(w => stateVars.exists(v => v.name.name == w))
      .map(Symbol(_))
      .toList

    GlobalLet(
      name = procname,
      funBody = FunBody(
        params = params,
        returnType = Some(unitType()),
        specs = List()
          ++ wellformedConditions().map(Requires)
          ++ invariants.map(Requires)
          ++ List(
          Writes(writes),
          Reads(stateVars.map(g => IdentifierExpr(g.name))))
          ++ invariants.map(Ensures),
        body = bodyWithLocals
      )
    )
  }


  //  def localsInPatterns(pattern: InExpr): List[Term] = pattern match {
  //    case VarUse(source, typ, name) =>
  //      List(LocalVar(name, transformTypeExpr(typ)))
  //    case TypedAst.FunctionCall(source, typ, functionName, args) =>
  //      args.flatMap(localsInPatterns)
  //    case _ =>
  //      List()
  //  }

  def transformVariableToTypeParam(variable: InVariable): TypedParam =
    TypedParam(variable.name.name, transformTypeExpr(variable.typ))


  def transformVariable(variable: InVariable): TypedParam =
    TypedParam(variable.name.name, transformTypeExpr(variable.typ))

  def transformParamToTypeParam(variable: Param): TypedParam =
    TypedParam(variable.name, transformTypeExpr(variable.typ))


  def transformBlockStmt(context: BlockStmt)(implicit ctxt: Context): Term = {
    makeBlockL(context.stmts.map(transformStatement))
  }

  def transformAtomicStmt(context: Atomic)(implicit ctxt: Context): Term = makeBlock(
    FunctionCall(beginAtomic, List(newInvocationId)),
    captureState(context, "begin atomic"),
    transformStatement(context.body)(ctxt.copy(isInAtomic = true)),
    captureState(context, "before commit"),
    FunctionCall(endAtomic, List(unit())).setTrace(EndAtomicTraceInfo(context)),
    captureState(context, "end atomic", context.source.stop)
  )

  //  def transformLocalVar(context: TypedAst.InVariable): LocalVar = {
  //    val v = transformVariable(context)
  //    LocalVar(v.name, v.typ)
  //  }


  def transformIfStmt(context: TypedAst.IfStmt)(implicit ctxt: Context): Term = {
    Conditional(transformExpr(context.cond),
      transformStatement(context.thenStmt),
      transformStatement(context.elseStmt))
  }


  val newInvocationId: Symbol = "new" + InvocationId

  def transformCrdtCall(context: CrdtCall)(implicit ctxt: Context): Term = {
    makeBlock(
      FunctionCall(crdtOperation, List(newInvocationId, transformFunctioncall(context.call))),
      unit())
  }

  def transformAssignment(context: TypedAst.Assignment)(implicit ctxt: Context): Term = {
    Assignment(context.varname.name, transformExpr(context.expr))
  }

  def transformStatement(stmt: InStatement)(implicit ctxt: Context): Term = {
    if (stmt == null)
      return makeBlock()
    makeBlock(
      captureState(stmt),
      transformStatement2(stmt).setTrace(AstElementTraceInfo(stmt)))
  }

  def captureState(elem: TypedAst.AstElem, msg: String = "", psource: SourcePosition = null): Term = {
    makeBlock() // TODO add comment or so
    //    val source = if (psource == null) elem.getSource().start else psource
    //    Assume(BoolConst(true), List(Attribute("captureState", List(Left("[line " + source.line + ":" + source.column + "] " + msg)))))
    //      .setTrace(AstElementTraceInfo(elem))
  }

  def transformStatement2(stmt: InStatement)(implicit ctxt: Context): Term = stmt match {
    case b@BlockStmt(source, stmts) =>
      transformBlockStmt(b)
    case a@Atomic(source, body) =>
      transformAtomicStmt(a)
    case l@TypedAst.LocalVar(source, variable) =>
      // was already translated at beginning of procedure
      makeBlock()
    case i@TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      transformIfStmt(i)
    case m@TypedAst.MatchStmt(source, expr, cases) =>
      transformMatchStmt(m)
    case c@CrdtCall(source, call) =>
      transformCrdtCall(c)
    case a@TypedAst.Assignment(source, varname, expr) =>
      transformAssignment(a)
    case n@NewIdStmt(source, varname, typename) =>
      transformNewIdStmt(n)
    case r: ReturnStmt =>
      transformReturnStmt(r)
    case AssertStmt(source, expr) =>
      Assert(transformExpr(expr)(ctxt.copy(targetIsLogic = true)))
  }


  def transformMatchStmt(m: MatchStmt)(implicit ctxt: Context): Term = {
    val e = transformExpr(m.expr)

    val cases: List[TermCase] =
      for (c <- m.cases) yield {
        TermCase(
          pattern = transformPattern(c.pattern),
          term = transformStatement(c.statement)
        )
      }
    MatchTerm(List(e), cases)
  }

  def transformPattern(p: InExpr): Pattern = {
    ???
  }


  def transformExpr(e: InExpr)(implicit ctxt: Context): Term = {
    val res = e match {
      case VarUse(source, typ, name) =>
        var va: Term = IdentifierExpr(name)
        if (ctxt.isRefVar(name)) {
          va = va.deref()
        }
        va
      case TypedAst.BoolConst(_, _, boolVal) =>
        WhyAst.BoolConst(boolVal)
      case TypedAst.IntConst(_, _, intVal) =>
        WhyAst.IntConst(intVal)
      case fc: TypedAst.FunctionCall =>
        transformFunctioncall(fc)
      case ab@ApplyBuiltin(source, typ, function, args) =>
        transformApplyBuiltin(ab)
      case qe@QuantifierExpr(source, typ, quantifier, vars, expr) =>
        transformQuantifierExpr(qe)
      case qe: InAllValidSnapshots =>
        ???
    }
    res.setTrace(AstElementTraceInfo(e))
  }

  private def makeDistinct(args: List[Expr]): List[Expr] = args match {
    case Nil => List()
    case x :: xs => (for (y <- xs) yield FunctionCall("=", List(x, y))) ++ makeDistinct(xs)
  }

  private def conjunction(list: List[Expr]): Expr = list match {
    case Nil => WhyAst.BoolConst(true)
    case _ =>
      list.reduce((x, y) => FunctionCall("&&", List(x, y)))
  }


  def transformApplyBuiltin(ab: ApplyBuiltin)(implicit ctxt: Context): Expr = {
    val args = ab.args.map(transformExpr)
    ab.function match {
      case BF_happensBefore(on) =>
        on match {
          case HappensBeforeOn.Unknown() =>
            throw new RuntimeException("must be typed")
          case HappensBeforeOn.Call() =>
            happensBefore(args.head, args(1))
          case HappensBeforeOn.Invoc() =>
            state_invocationHappensBefore.get(args.head, args(1))
        }
      case BF_sameTransaction() =>
        sameTransaction(args.head, args(1))
      case BF_isVisible() =>
        state_visiblecalls.get(args.head)
      case BF_less() =>
        FunctionCall("<", args)
      case BF_lessEq() =>
        FunctionCall("<=", args)
      case BF_greater() =>
        FunctionCall(">", args)
      case BF_greaterEq() =>
        FunctionCall(">=", args)
      case BF_equals() =>
        FunctionCall("=", args)
      case BF_notEquals() =>
        FunctionCall("not", List(FunctionCall("=", args)))
      case BF_and() =>
        FunctionCall("&&", args)
      case BF_or() =>
        FunctionCall("||", args)
      case BF_implies() =>
        FunctionCall("->", args)
      case BF_not() =>
        FunctionCall("not", args)
      case BF_plus() =>
        FunctionCall("+", args)
      case BF_minus() =>
        FunctionCall("-", args)
      case BF_mult() =>
        FunctionCall("*", args)
      case BF_div() =>
        FunctionCall("/", args)
      case BF_mod() =>
        FunctionCall("mod", args)
      case BF_getOperation() =>
        state_callops.get(args.head)
      case BF_getInfo() =>
        state_invocations.get(args.head)
      case BF_getResult() =>
        state_invocationResult.get(args.head)
      case BF_getOrigin() =>
        state_origin.get(args.head)
      case BF_getTransaction() =>
        state_callTransaction.get(args.head)
      case BF_inCurrentInvoc() =>
        state_origin.get(args.head) === "new" + InvocationId
      case BF_distinct() =>
        conjunction(makeDistinct(args))
      //        Lookup("old_state_inCurrentInvocation" else "state_inCurrentInvocation", args)
    }
  }

  //  def transformExpr(e: InExpr): Expr = {
  //    if (e.varname != null) {
  //      IdentifierExpr(e.varname.getText)
  //    } else if (e.operator != null) {
  //      e.operator.getText match {
  //        case "before" =>
  //          Lookup("state_happensBefore", List(transformExpr(e.left), transformExpr(e.right)))
  //        case "after" =>
  //          Lookup("state_happensBefore", List(transformExpr(e.right), transformExpr(e.left)))
  //        case op =>
  //          FunctionCall(op, List(transformExpr(e.left), transformExpr(e.right)))
  //      }
  //    } else if (e.quantifierExpr() != null) {
  //      transformQuantifierExpr(e.quantifierExpr())
  //    } else if (e.functionCall() != null) {
  //      transformFunctioncall(e.functionCall())
  //    } else if (e.parenExpr != null) {
  //      transformExpr(e.parenExpr)
  //    } else if (e.isAttribute != null) {
  //      Lookup("state_visibleCalls", List(transformExpr(e.left)))
  //    } else if (e.receiver != null) {
  //      val receiver = transformExpr(e.receiver)
  //      e.fieldName.getText match {
  //        case "op" => Lookup("state_callOps", List(receiver))
  //        case "info" => Lookup("state_invocations", List(receiver))
  //        case "origin" => Lookup("state_origin", List(receiver))
  //      }
  //    } else if (e.unaryOperator != null) {
  //      FunctionCall(e.unaryOperator.getText, List(transformExpr(e.right)))
  //    } else {
  //      throw new RuntimeException("unhandled case: " + e.toStringTree(parser))
  //    }
  //  }

  def transformNewIdStmt(context: NewIdStmt): Term = {
    val varName: String = context.varname.name
    val typ = transformTypeExpr(context.typename)
    makeBlock(
      // nondeterministic creation of new id
      Havoc(varName, typ),
      // we can assume that the new id was never used in an operation before
      newIdAssumptions(typ, varName) // TODO move into any-specs
    )

  }

  def Havoc(varName: String, typ: TypeExpression) = Assignment(varName, AnyTerm(typ))

  def newIdAssumptions(typeName: TypeExpression, idName: String): Term = {
    // add axioms for contained ids
    var result = List[Term]()

    val generated = state_locallyGenerated(typeName.stringName)
    result ++= List(
      Assignment(generated, "Map.set".call(generated.deref(), IdentifierExpr(idName).deref(), WhyAst.BoolConst(true)))
    )

    for ((opName, args2) <- operationDefs) {
      val args = args2.map(v => v.copy(name = "_p_" + v.name))
      val idType = typeName
      val argIds: List[Symbol] = args.map(a => IdentifierExpr(a.name))
      result = result ++ (for (arg <- args; if arg.typ == idType) yield {
        Assume(Forall(("_c" :: typeCallId) +: args,
          (state_callops.get("_c") === FunctionCall(operationCaseName(opName), argIds))
            ==> (IdentifierExpr(idName).deref() !== IdentifierExpr(arg.name))))
      })
    }


    makeBlockL(result)
  }

  def transformReturnStmt(context: TypedAst.ReturnStmt)(implicit ctxt: Context): Term = {
    val returnedExpr: Expr = transformExpr(context.expr)
    makeReturn(Some(returnedExpr), context.assertions, context)
  }


  def makeReturn(returnedExpr: Option[Expr], endAssertions: List[AssertStmt], source: TypedAst.AstElem)(implicit ctxt: Context): Term = {
    val procRes = FunctionCall(invocationResForProc(ctxt.procedureName), returnedExpr.toList)

    makeBlock(
      if (ctxt.isInAtomic) {
        FunctionCall(endAtomic, List()).setTrace(EndAtomicTraceInfo(source))
      } else {
        makeBlock()
      },
      captureState(source, s"before return"),
      FunctionCall(finishInvocation, List("new" + InvocationId, procRes)),
      makeBlockL(
        endAssertions.map(transformStatement(_)(ctxt.copy(useOldCurrentInvocation = true))) // TODO add old current invocation
      ),
      returnedExpr match {
        case None => Tuple(List())
        case Some(e) => e
      }
    )
  }

  def transformFunctioncall(context: TypedAst.FunctionCall)(implicit ctxt: Context): FunctionCall = {
    var funcName: String = context.functionName.name
    var args: List[Expr] = context.args.toList.map(transformExpr)

    funcName = functionReplacements.getOrElse(funcName, funcName)

    if (queryFunctions.contains(funcName)) {
      // add state vars for query-functions
      args ++= stateVars.map(g => IdentifierExpr(g.name))

      if (!ctxt.targetIsLogic) {
        funcName = s"${funcName}_proc"
      }

    } else if (procedureNames.contains(funcName)) {
      // add invocation name
      funcName = invocationInfoForProc(funcName)
    }

    FunctionCall(funcName, args)
  }

  def transformQuantifierExpr(q: TypedAst.QuantifierExpr)(implicit ctxt: Context): Expr = {
    val vars = q.vars.toList.map(transformVariable)
    val e = transformExpr(q.expr)
    q.quantifier match {
      case InputAst.Exists() => Exists(vars, e)
      case InputAst.Forall() => Forall(vars, e)
    }
  }

  def transformTypeExpr(t: Option[InTypeExpr]): Option[TypeExpression] = t.map(transformTypeExpr)

  def transformTypeExpr(t: InTypeExpr): TypeExpression = t match {
    case AnyType() => ???
    case BoolType() => TypeBool()
    case IntType() => TypeSymbol("int")
    case CallIdType() => typeCallId
    case InvocationIdType() => typeInvocationId
    case TransactionIdType() => typeTransactionId
    case InvocationInfoType() => typeInvocationInfo
    case InvocationResultType() => typeInvocationResult
    case SomeOperationType() => typeOperation
    case OperationType(name) => TypeSymbol(operation)
    case TypedAst.FunctionType(argTypes, returnType, kind) => ???
    case TypedAst.SimpleType(name, _) => TypeSymbol(typeName(name))
    case IdType(name) => TypeSymbol(typeName(name))
  }

  //  def transformTypeExpr(t: InTypeExpr): TypeExpression = {
  //    val typeName: String = t.name.getText
  //    if (typeName == "Boolean") {
  //      TypeBool()
  //    } else {
  //      TypeSymbol(typeName)
  //    }
  //
  //  }

}
