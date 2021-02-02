package crdtver.language

import crdtver.language.InputAst.BuiltInFunc
import crdtver.language.InputAst.BuiltInFunc.BF_distinct
import crdtver.language.TypedAst.FunctionKind.FunctionKindCrdtQuery
import crdtver.language.TypedAst.{TypeVarUse, UnitType}
import crdtver.language.crdts.ACrdtInstance
import crdtver.utils.PrettyPrintDoc.Doc
;

object TypedAstPrinter {

  import crdtver.utils.PrettyPrintDoc._

  def printCrdt(programCrdt: ACrdtInstance): Doc = ""

  def printDeclaration(declaration: TypedAst.InDeclaration): Doc = declaration match {
    case TypedAst.InProcedure(source, name, params, locals, returnType, body) =>
      "def" <+> name.name + "(" <> sep(", ", params.map(print)) <> "): " <>
        printType(returnType) <+> "{" <>
        nested(2,
          line <> sep(line, locals.map(l => "var " <> print(l))) </>
            print(body)
        ) </>
        "}"
    case TypedAst.InTypeDecl(source, isIdType, name, tps, dataTypeCases) =>
      (if (isIdType) {
        "idtype"
      } else {
        "type"
      }) <+> name.name <+>
        (if (tps.isEmpty) "" else "[" <> sep(", ", tps.map(_.customToString)) <> "]") <+>
        (if (dataTypeCases.isEmpty) {
          ""
        } else {
          " = " <>
            nested(2,
              line <> "  " <> sep(line <> "| ", dataTypeCases.map(print))
            )
        })
    case TypedAst.InOperationDecl(source, name, params) =>
      "operation" <+> name.name <> "(" <> sep(", ", params.map(print)) <> ")"
    case TypedAst.InQueryDecl(source, name, params, returnType, implementation, ensures, annotations) =>
      "query" <+> name.name <> "(" <> sep(", ", params.map(print)) <> "): " <> printType(returnType)
    case TypedAst.InAxiomDecl(source, expr) =>
      "axiom" <+> printExpr(expr)
    case TypedAst.InInvariantDecl(source, name, isFree, expr, _) =>
      (if (isFree) "free " else "") <> "invariant" <+> name <+> printExpr(expr)
    case TypedAst.InCrdtDecl(source, keyDecl) =>
      "crdt" <+> print(keyDecl)
  }

  def printCrdtType(crdtType: TypedAst.InCrdtType): Doc = crdtType match {
    case TypedAst.InCrdt(source, name, typ) =>
      name.name <> "<" <> sep(", ", typ.map(print)) <> ">"
    case TypedAst.InStructCrdt(source, keyDecl) =>
      "{" <> nested(2, line <> sep("," <> line, keyDecl.map(k => print(k) <> line))) </> "}"
  }

  def printOp(left: TypedAst.InExpr, op: String, right: TypedAst.InExpr): Doc = {
    val l = printExpr(left)
    val r = printExpr(right)
    group(nested(2, "(" <> l <> lineOrSpace <> op <+> r <> ")"))
  }

  def printTypeArgs(args: List[TypedAst.InTypeExpr]): Doc =
    if (args.isEmpty) ""
    else "[" <> sep(", ", args.map(print)) <> "]"

  def printVar(v: TypedAst.InVariable): Doc =
    v.name.name <> ": " <> print(v.typ)

  def printExpr(expr: TypedAst.InExpr): Doc = expr match {
    case TypedAst.VarUse(source, typ, name) =>
      name
    case TypedAst.BoolConst(source, typ, value) =>
      value.toString
    case TypedAst.IntConst(source, typ, value) =>
      value.toString()
    case TypedAst.CrdtQuery(source, typ, qryOp) =>
      group("crdtQuery" <+> print(qryOp))
    case expr: TypedAst.CallExpr =>
      expr match {
        case TypedAst.FunctionCall(source, typ, functionName, typeArgs, args, kind) =>
          val k = if (kind == FunctionKindCrdtQuery()) "query " else ""
          group(k <> functionName.name <> printTypeArgs(typeArgs) <> "(" <> nested(2, line <> sep(", ", args.map(e => printExpr(e) <> line)) <> ")"))
        case TypedAst.ApplyBuiltin(source, typ, function, args) =>
          function match {
            case BuiltInFunc.BF_isVisible() =>
              printExpr(args(0)) <> ".isVisible"
            case BuiltInFunc.BF_upperBoundedBy() =>
              printOp(args(0), "upperBound", args(1))
            case BuiltInFunc.BF_happensBefore(on) =>
              printOp(args(0), "happensBefore", args(1))
            case BuiltInFunc.BF_sameTransaction() =>
              printOp(args(0), "inSameTransactionAs", args(1))
            case BuiltInFunc.BF_less() =>
              printOp(args(0), "<", args(1))
            case BuiltInFunc.BF_lessEq() =>
              printOp(args(0), "<=", args(1))
            case BuiltInFunc.BF_greater() =>
              printOp(args(0), ">", args(1))
            case BuiltInFunc.BF_greaterEq() =>
              printOp(args(0), ">=", args(1))
            case BuiltInFunc.BF_equals() =>
              printOp(args(0), "==", args(1))
            case BuiltInFunc.BF_notEquals() =>
              printOp(args(0), "!=", args(1))
            case BuiltInFunc.BF_and() =>
              printOp(args(0), "&&", args(1))
            case BuiltInFunc.BF_or() =>
              printOp(args(0), "||", args(1))
            case BuiltInFunc.BF_implies() =>
              printOp(args(0), "==>", args(1))
            case BuiltInFunc.BF_iff() =>
              printOp(args(0), "<==>", args(1))
            case BuiltInFunc.BF_not() =>
              "!" <> printExpr(args(0))
            case BuiltInFunc.BF_plus() =>
              printOp(args(0), "+", args(1))
            case BuiltInFunc.BF_minus() =>
              printOp(args(0), "-", args(1))
            case BuiltInFunc.BF_mult() =>
              printOp(args(0), "*", args(1))
            case BuiltInFunc.BF_div() =>
              printOp(args(0), "/", args(1))
            case BuiltInFunc.BF_mod() =>
              printOp(args(0), "%", args(1))
            case BuiltInFunc.BF_getOperation() =>
              printExpr(args(0)) <> ".op"
            case BuiltInFunc.BF_getInfo() =>
              printExpr(args(0)) <> ".info"
            case BuiltInFunc.BF_getResult() =>
              printExpr(args(0)) <> ".result"
            case BuiltInFunc.BF_getOrigin() =>
              printExpr(args(0)) <> ".origin"
            case BuiltInFunc.BF_getTransaction() =>
              printExpr(args(0)) <> ".tx"
            case BuiltInFunc.BF_inCurrentInvoc() =>
              printExpr(args(0)) <> ".inCurrentInvoc"
            case BF_distinct() =>
              functionCall("distinct", args.map(printExpr))
          }
      }
    case TypedAst.QuantifierExpr(source, quantifier, vars, expr) =>
      val q: Doc = quantifier match {
        case InputAst.Forall() => "forall"
        case InputAst.Exists() => "exists"
      }
      group(nested(4, "(" <> q <+> sep(", ", vars.map(printVar)) <+> "::" </> print(expr) <> ")"))
    case TypedAst.AggregateExpr(source, op, vars, filter, elem) =>
          val q: Doc = op match {
            case TypedAst.Sum() => "sum"
          }
          group(nested(4, "(" <> q <+> sep(", ", vars.map(printVar)) <+> "::" </> print(elem) </> " where " <> print(filter) <> ")"))
    case TypedAst.InAllValidSnapshots(_, expr) =>
      group("(forall valid snapshots :: " </> nested(4, print(expr)) <> ")")
  }

  def printStatement(statement: TypedAst.InStatement): Doc = statement match {
    case TypedAst.BlockStmt(source, stmts) =>
      "{" <>
        nested(2, line <> sep(line, stmts.map(printStatement))) </>
        "}"
    case TypedAst.Atomic(source, body) =>
      "atomic" <+> printStatement(body)
    case TypedAst.LocalVar(source, variable) =>
      "var" <+> print(variable)
    case TypedAst.IfStmt(source, cond, thenStmt, elseStmt) =>
      "if (" <> printExpr(cond) <> ") " <> printStatement(thenStmt) <+> "else" <+> printStatement(elseStmt)
    case TypedAst.MatchStmt(source, expr, cases) =>
      printExpr(expr) <+> "match {" </>
        nested(2, sep(line, cases.map(c => print(c) <> line))) <>
        "}"
    case TypedAst.CrdtCall(source, call) =>
      "call" <+> print(call)
    case TypedAst.Assignment(source, varname, expr) =>
      varname.name <+> "=" <+> printExpr(expr)
    case TypedAst.NewIdStmt(source, varname, typename) =>
      "var" <+> varname.name <+> " = new " <> printType(typename)
    case TypedAst.ReturnStmt(source, expr, assertions) =>
      "return" <+> printExpr(expr) <>
        nested(2, sep("", assertions.map(a => line <> "assert" <+> print(a))))
    case TypedAst.AssertStmt(source, expr) =>
      "assert" <+> printExpr(expr)
  }

  def printType(typ: TypedAst.InTypeExpr): Doc = typ match {
    case TypedAst.AnyType() => "any"
    case TypedAst.BoolType() => "Bool"
    case TypedAst.IntType() => "Int"
    case TypedAst.CallIdType() => "CallId"
    case TypedAst.CallInfoType() => "CallInfo"
    case TypedAst.InvocationIdType() => "InvocationId"
    case TypedAst.TransactionIdType() => "TransactionId"
    case TypedAst.InvocationInfoType() => "InvocationInfo"
    case TypedAst.InvocationResultType() => "InvocationResult"
    case TypedAst.SomeOperationType() => "SomeOperation"
    case TypedAst.OperationType(name) => "Operation<" <> name <> ">"
    case TypedAst.FunctionType(argTypes, returnType, functionKind) =>
      "(" <> sep(", ", argTypes.map(printType)) <> ") => " <> printType(returnType)
    case TypedAst.SimpleType(name, typeArgs) =>
      name <> (typeArgs match {
        case List() => ""
        case _ => "[" <> sep(", ", typeArgs.map(printType)) <> "]"
      })
    case TypedAst.IdType(name) =>
      name
    case TypeVarUse(name) =>
      name
    case UnitType() =>
      "Unit"
  }

  def print(elem: TypedAst.AstElem): Doc = elem match {
    case TypedAst.InProgram(name, source, procedures, types, axioms, invariants, programCrdt) =>
      sep(line, types.map(print)) </>
        sep(line, axioms.map(print)) </>
        sep(line, procedures.map(print)) </>
        sep(line, invariants.map(print)) </>
        printCrdt(programCrdt)
    case declaration: TypedAst.InDeclaration =>
      printDeclaration(declaration)
    case TypedAst.DataTypeCase(source, name, params) =>
      name.name <> "(" <> sep(", ", params.map(print)) <> ")"
    case TypedAst.InKeyDecl(source, name, crdttype) =>
      name.name <> ":" <+> print(crdttype)
    case crdtType: TypedAst.InCrdtType =>
      printCrdtType(crdtType)
    case TypedAst.InVariable(source, name, typ) =>
      name.name <> ":" <+> print(typ)
    case expr: TypedAst.InExpr =>
      printExpr(expr)
    case statement: TypedAst.InStatement =>
      printStatement(statement)
    case TypedAst.MatchCase(source, pattern, statement) =>
      "case" <+> print(pattern) <+> "=>" </> printStatement(statement)
    case typ: TypedAst.InTypeExpr =>
      printType(typ)
    case TypedAst.TypeParameter(source, name) =>
      name.name
  }


}
