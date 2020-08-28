package crdtver.symbolic

import crdtver.language.TypedAst.InTypeExpr
import crdtver.symbolic.SVal.{SymbolicMap, SymbolicSet}
import crdtver.utils.PrettyPrintDoc
import crdtver.utils.PrettyPrintDoc.sep
import edu.nyu.acsys.CVC4.Kind

import scala.language.existentials

/** a symbolic value
 *
 * the type parameter T is the sort of the value.
 * It is a phantom type (only used for type checking in the DSL)
 *
 * */
sealed abstract class SVal[T <: SymbolicSort] {
  def upcast[S >: T <: SymbolicSort]: SVal[S] = this.asInstanceOf[SVal[S]]

  def cast[S <: SymbolicSort](implicit sort: S): SVal[S] = {
    require(typ == sort, s"Cannot cast $typ to $sort")
    this.asInstanceOf[SVal[S]]
  }

  def castUnsafe[S <: SymbolicSort]: SVal[S] = this.asInstanceOf[SVal[S]]

  def typ: T

  def ===(other: SVal[T]): SVal[SortBoolean] = SEq(this, other)

  def !==(other: SVal[T]): SVal[SortBoolean] = SNotEq(this, other)

  def children: List[SVal[_ <: SymbolicSort]] =
    childrenP._1

  def childrenT: List[SymbolicSort] =
    childrenP._2

  def childrenP: (List[SVal[_ <: SymbolicSort]], List[SymbolicSort]) = this.asInstanceOf[SVal[_]] match {
    case ConcreteVal(_) =>
      (List(), List())
    case SymbolicVariable(_, _, typ) =>
      (List(), List(typ))
    case SEq(left, right) =>
      (List(left, right), List())
    case SNotEq(left, right) =>
      (List(left, right), List())
    case SLessThan(left, right) =>
      (List(left, right), List())
    case SLessThanOrEqual(left, right) =>
      (List(left, right), List())
    case SDistinct(values) =>
      (values, List())
    case SNone(ofTyp) =>
      (List(), List(ofTyp))
    case SSome(value) =>
      (List(value), List())
    case SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
      (List(option, ifSome, ifNone).asInstanceOf[List[SVal[_ <: SymbolicSort]]], List(ifSomeVariable.typ))
    case SReturnVal(methodName, value) =>
      (List(value), List())
    case SReturnValNone() =>
      (List(), List())
    case SMapGet(map, key) =>
      (List(map, key), List())
    case SymbolicMapEmpty(kt, d) =>
      (List(d), List(kt))
    case SymbolicMapUpdated(k, v, b) =>
      (List(k, v, b), List())
    case SSetUnion(a, b) =>
      (List(a, b), List())
    case SSetInsert(a, b) =>
      (List(a) ++ b.toList, List())
    case SSetEmpty(t) =>
      (List(), List(t))
    case SSetVar(v) =>
      (List(v), List())
    case SSetContains(set, value) =>
      (List(set, value), List())
    case QuantifierExpr(quantifier, variable, body) =>
      (List(body), List(variable.typ))
    case SAggregateExpr(op, variables, filter, elem) =>
      (List(filter, elem), variables.map(_.typ))
    case SCommitted() =>
      (List(), List())
    case SUncommitted() =>
      (List(), List())
    case SBool(value) =>
      (List(), List())
    case SNot(value) =>
      (List(value), List())
    case SAnd(left, right) =>
      (List(left, right), List())
    case SOr(left, right) =>
      (List(left, right), List())
    case SImplies(left, right) =>
      (List(left, right), List())
    case SFunctionCall(typ, functionName, args) =>
      (args, List())
    case SDatatypeValue(inType, constructorName, values, dtyp) =>
      (values, List(dtyp))
    case SCallInfo(operationName, args) =>
      (args, List())
    case SCallInfoNone() =>
      (List(), List())
    case SInvocationInfo(procname, args) =>
      (args, List())
    case SInvocationInfoNone() =>
      (List(), List())
    case MapDomain(map) =>
      (List(map), List())
    case IsSubsetOf(left, right) =>
      (List(left, right), List())
    case SValOpaque(v, t) =>
      (List(), List(t))
    case SNamedVal(_, v) => (List(v), List())
    case SChooseSome(_, v) => (List(v), List())
    case SBinaryInt(_, l, r) =>
      (List(l,r), List())
  }

  def printIntOp(op: SIntOp): String = op match {
    case SPlus() => "+"
    case SMinus() => "-"
    case SMult() => "*"
    case SDiv() => "/"
    case SMod() => "%"
  }

  def prettyPrint: PrettyPrintDoc.Doc = {
    import PrettyPrintDoc._

    def printOp(l: SVal[_], op: String, r: SVal[_]): Doc =
      ("(" <> l.prettyPrint <+> op <+> r.prettyPrint <> ")") :<|>
        (() => "(" <> l.prettyPrint <+> line <> op <+> r.prettyPrint <> ")")

    this.asInstanceOf[SVal[_]] match {
      case ConcreteVal(value) =>
        value.toString
      case SymbolicVariable(name, _, typ) =>
        name
      case SEq(left, right) =>
        printOp(left, "==", right)
      case SNotEq(left, right) =>
        printOp(left, "!=", right)
      case SLessThan(left, right) =>
        printOp(left, "<", right)
      case SLessThanOrEqual(left, right) =>
        printOp(left, "<=", right)
      case SNone(t) =>
        s"None<$t>"
      case SSome(value) =>
        "Some(" <> value.prettyPrint <> ")"
      case SOptionMatch(option, ifSomeVariable, ifSome, ifNone) =>
        group("(match " <> option.prettyPrint <> " with" </>
          nested(2, "| None => " <> ifNone.prettyPrint) </>
          nested(2, "| Some(" <> ifSomeVariable.prettyPrint <> ") => " <> ifSome.prettyPrint) <> ")")
      case SReturnVal(methodName, value) =>
        "(return " <> methodName <+> value.prettyPrint <> ")"
      case SMapGet(map, key) =>
        map.prettyPrint <> "[" <> key.prettyPrint <> "]"
      case SymbolicMapEmpty(kt, defaultValue) =>
        "empty[" <> kt.toString <> "](default := " <> defaultValue.prettyPrint <> ")"
      case SymbolicMapUpdated(updatedKey, newValue, baseMap) =>
        baseMap.prettyPrint <> "(" <> updatedKey.prettyPrint <+> ":=" <+> newValue.prettyPrint <> ")"
      case SSetVar(v) =>
        v.prettyPrint
      case SSetEmpty(t) =>
        "{} :: " <> t.toString
      case SSetInsert(SSetEmpty(_), v) =>
        "{" <> sep(", ", v.toList.map(_.prettyPrint)) <> "})"
      case SSetInsert(set, v) =>
        "(" <> set.prettyPrint <> " ∪ {" <> sep(", ", v.toList.map(_.prettyPrint)) <> "})"
      case SSetUnion(a, b) =>
        "(" <> a.prettyPrint <> " ∪ " <> b.prettyPrint <> ")"
      case SSetContains(set, value) =>
        printOp(set, "contains", value)
      case QuantifierExpr(quantifier, variable, body) =>
        (quantifier.toString <> variable.name <> ":" <+> variable.typ.toString <+> "::" <+> body.prettyPrint) :<|>
          (() => quantifier.toString <> variable.name <> ":" <+> variable.typ.toString <+> "::" </> nested(2, body.prettyPrint))
      case SAggregateExpr(op, variables, filter, elem) =>
        group("(" <> op.toString <> sep(", ", variables.map(_.prettyPrint)) <> nested(2, "::" </>
          elem.prettyPrint <> " where " <> filter.prettyPrint <> ")"))
      case SCommitted() =>
        "committed"
      case SUncommitted() =>
        "uncommitted"
      case SBool(value) =>
        value.toString
      case SNot(value) =>
        "!" <> value.prettyPrint
      case SAnd(left, right) =>
        printOp(left, "&&", right)
      case SOr(left, right) =>
        printOp(left, "||", right)
      case SImplies(left, right) =>
        printOp(left, "==>", right)
      case SFunctionCall(typ, f, args) =>
        f.name <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SDatatypeValue(inType, constructorName, values, _) =>
        constructorName <> "(" <> sep(",", values.map(_.prettyPrint)) <> ")"
      case SInvocationInfo(procname, args) =>
        procname <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SInvocationInfoNone() =>
        "no_invocation"
      case SReturnValNone() =>
        "not_returned"
      case MapDomain(map) =>
        "dom(" <> map.prettyPrint <> ")"
      case IsSubsetOf(left, right) =>
        printOp(left, "⊆", right)
      case SDistinct(args) =>
        "distinct(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SCallInfo(op, args) =>
        op <> "(" <> sep(",", args.map(_.prettyPrint)) <> ")"
      case SCallInfoNone() =>
        "NoCall"
      case SValOpaque(v, t) =>
        //        s"OPAQUE($k, $v, $t)"
        v.toString
      case SNamedVal(name, v) =>
        "(*" <+> name <+> "*) " <> v.prettyPrint
      case SChooseSome(cond, v) =>
        "(CHOOSE " <> cond.prettyPrint <> " return " <> v.prettyPrint <> ")"
      case SBinaryInt(op, left, right) =>
        printOp(left, printIntOp(op), right)
    }
  }

  def defaultToString: String = super.toString

  override def toString: String =
    prettyPrint.prettyStr(120)

}

object SVal {
  type SymbolicSet[T <: SymbolicSort] = SVal[SortSet[T]]
  type SymbolicMap[K <: SymbolicSort, V <: SymbolicSort] = SVal[SortMap[K, V]]


  def makeSet[T <: SymbolicSort](values: Iterable[SVal[T]])(implicit t: T): SVal[SortSet[T]] =
    SSetInsert(SSetEmpty(t), values.toSet)

  def and(exprs: List[SVal[SortBoolean]]): SVal[SortBoolean] = exprs match {
    case List() => SBool(true)
    case _ => exprs.reduce(SAnd)
  }

  def forall[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QForall(), variable, body)

  def forallL(variables: List[SymbolicVariable[_ <: SymbolicSort]], body: SVal[SortBoolean]): SVal[SortBoolean] =
    variables.foldRight(body)(forall(_, _))

  def exists[T <: SymbolicSort](variable: SymbolicVariable[T], body: SVal[SortBoolean]): QuantifierExpr =
    QuantifierExpr(QExists(), variable, body)

  def existsL(variables: List[SymbolicVariable[_ <: SymbolicSort]], body: SVal[SortBoolean]): SVal[SortBoolean] =
    variables.foldRight(body)(exists(_, _))

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: SVal[SortValue]*)(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args.toList, t)

  def datatype(typ: InTypeExpr, name: String, t: SortDatatype, args: List[SVal[SortValue]])(implicit ctxt: SymbolicContext): SVal[SortDatatype] =
    SDatatypeValue(ctxt.translateSortDatatypeToImpl(typ), name, args, t)

  def unionL[T <: SymbolicSort](sets: List[SVal[SortSet[T]]])(implicit t: T): SVal[SortSet[T]] =
    sets.foldLeft[SVal[SortSet[T]]](SSetEmpty(t))(_ union _)

  implicit class MapGetExtension[K <: SymbolicSort, V <: SymbolicSort](mapExpr: SymbolicMap[K, V]) {
    def apply(key: SVal[K]): SMapGet[K, V] = SMapGet(mapExpr, key)

    def put(key: SVal[K], value: SVal[V]): SymbolicMap[K, V] = {
      SymbolicMapUpdated[K, V](
        updatedKey = key,
        newValue = value,
        baseMap = mapExpr
      )
    }

    def get(key: SVal[K]): SVal[V] =
      SMapGet(mapExpr, key)

  }

  implicit class BooleanSValExtensions(left: SVal[SortBoolean]) {
    def -->(right: SVal[SortBoolean]): SVal[SortBoolean] = SImplies(left, right)

    def <-->(right: SVal[SortBoolean]): SVal[SortBoolean] = SEq(left, right)

    def &&(right: SVal[SortBoolean]): SVal[SortBoolean] = SAnd(left, right)

    def ||(right: SVal[SortBoolean]): SVal[SortBoolean] = SOr(left, right)

    def unary_!(): SVal[SortBoolean] = SNot(left)

  }

  implicit class SetSValExtensions[T <: SymbolicSort](left: SVal[SortSet[T]]) {
    def contains(right: SVal[T]): SVal[SortBoolean] = SSetContains(left, right)

    def subset(right: SVal[SortSet[T]]): SVal[SortBoolean] = isSubsetOf(right)

    def isSubsetOf(other: SVal[SortSet[T]]): SVal[SortBoolean] = {
      IsSubsetOf(left, other)
    }

    def +(c: SVal[T]): SVal[SortSet[T]] = left match {
      case SSetInsert(set, values) =>
        SSetInsert(set, values + c)
      case _ => SSetInsert(left, Set(c))
    }


    def union(other: SVal[SortSet[T]]): SVal[SortSet[T]] = (left, other) match {
      case (SSetEmpty(_), x) => x
      case (x, SSetEmpty(_)) => x
      case (SSetInsert(set1, values1), SSetInsert(set2, values2)) =>
        SSetInsert(set1.union(set2), values1 ++ values2)
      case (SSetInsert(set1, values), set2) =>
        SSetInsert(set1.union(set2), values)
      case (set2, SSetInsert(set1, values)) =>
        SSetInsert(set1.union(set2), values)
      case _ =>
        SSetUnion(left, other)
    }
  }


  implicit class CallExtensions(left: SVal[SortCallId]) {
    def happensBefore(right: SVal[SortCallId])(implicit state: SymbolicState): SVal[SortBoolean] =
      ExprTranslation.callHappensBefore(left, right)

    /** set of calls that happened before this one */
    def happensBeforeSet(implicit state: SymbolicState): SVal[SortSet[SortCallId]] =
      state.happensBefore.get(left)

    def inSameTransactionAs(right: SVal[SortCallId])(implicit state: SymbolicState): SVal[SortBoolean] =
      state.callOrigin.get(left) === state.callOrigin.get(right)

    def op(implicit state: SymbolicState): SVal[SortCall] =
      state.calls.get(left)

    def isVisible(implicit state: SymbolicState): SVal[SortBoolean] =
      state.visibleCalls.contains(left)


    def tx(implicit state: SymbolicState): SVal[SortOption[SortTxId]] =
      state.callOrigin.get(left)
  }

  implicit class TransactionExtensions(left: SVal[SortTxId]) {
    def invocation(implicit state: SymbolicState): SVal[SortOption[SortInvocationId]] =
      state.transactionOrigin.get(left)

  }

  implicit class InvocationExtensions(left: SVal[SortInvocationId]) {
    def happensBefore(right: SVal[SortInvocationId])(implicit ctxt: SymbolicContext, state: SymbolicState): SVal[SortBoolean] =
      ExprTranslation.invocationHappensBefore(left, right)

    def res(implicit state: SymbolicState): SVal[SortInvocationRes] =
      state.invocationRes.get(left)

    def op(implicit state: SymbolicState): SVal[SortInvocationInfo] =
      state.invocationOp.get(left)

    def calls(implicit state: SymbolicState): SVal[SortSet[SortCallId]] =
      state.invocationCalls.get(left)

  }

  implicit class OptionExtensions[T <: SymbolicSort](left: SVal[SortOption[T]]) {
    def isNone(implicit t: T): SVal[SortBoolean] = (left === SNone(t))

    def isDefined(implicit t: T): SVal[SortBoolean] = (left !== SNone(t))

  }

}


case class ConcreteVal[R, T <: SymbolicSort](value: R)(implicit val typ: T) extends SVal[T] {
}


case class SymbolicVariable[Sort <: SymbolicSort](
  name: String,
  isBound: Boolean,
  typ: Sort
) extends SVal[Sort] {
  override def toString: String = s"$name"
}

case class SEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean] {
  require(left.typ == right.typ, s"Incompatible types in $left == $right:  ${left.typ} and ${right.typ}")

  override def typ: SortBoolean = SortBoolean()
}

case class SNotEq[T <: SymbolicSort](left: SVal[T], right: SVal[T]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SLessThan(left: SVal[SortInt], right: SVal[SortInt]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SLessThanOrEqual(left: SVal[SortInt], right: SVal[SortInt]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SDistinct[T <: SymbolicSort](values: List[SVal[T]]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


case class SNone[T <: SymbolicSort](ofTyp: T) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(ofTyp)
}

case class SSome[T <: SymbolicSort](value: SVal[T]) extends SVal[SortOption[T]] {
  override def typ: SortOption[T] = SortOption(value.typ)
}

case class SOptionMatch[O <: SymbolicSort, T <: SymbolicSort](
  option: SVal[SortOption[O]],
  ifSomeVariable: SymbolicVariable[O],
  ifSome: SVal[T],
  ifNone: SVal[T]
)(implicit val typ: T) extends SVal[T] {
}

case class SChooseSome[T <: SymbolicSort](
  condition: SVal[SortBoolean],
  value: SymbolicVariable[T]
)(implicit val typ: T) extends SVal[T]

case class SReturnVal(methodName: String, value: SVal[SortValue]) extends SVal[SortInvocationRes] {
  override def typ: SortInvocationRes = SortInvocationRes()
}

case class SReturnValNone() extends SVal[SortInvocationRes] {
  override def typ: SortInvocationRes = SortInvocationRes()
}


// TODO could make application more generic and use Hlists (whooo!)
// case class SApp(func: SFunc, args: List[SVal[_]])


case class SMapGet[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, V]], key: SVal[K]) extends SVal[V] {
  override def typ: V = map.typ.valueSort
}


object SymbolicMapVar {
  def symbolicMapVar[K <: SymbolicSort, V <: SymbolicSort](name: String)
    (implicit keySort: K, valueSort: V, ctxt: SymbolicContext): SymbolicMap[K, V] =
    ctxt.makeVariable[SortMap[K, V]](name)
}


case class SymbolicMapEmpty[K <: SymbolicSort, V <: SymbolicSort](
  keySort: K,
  defaultValue: SVal[V]
)(implicit val valueSort: V) extends SymbolicMap[K, V] {
  override def typ: SortMap[K, V] = SortMap(keySort, valueSort)
}

// map updated with a symbolic key
case class SymbolicMapUpdated[K <: SymbolicSort, V <: SymbolicSort](
  updatedKey: SVal[K],
  newValue: SVal[V],
  baseMap: SVal[SortMap[K, V]]
) extends SymbolicMap[K, V] {

  override def typ: SortMap[K, V] = SortMap(updatedKey.typ, newValue.typ)
}


case class SSetVar[T <: SymbolicSort](variable: SVal[SortSet[T]]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = variable.typ
}

case class SSetEmpty[T <: SymbolicSort](t: T) extends SymbolicSet[T] {
  override def typ: SortSet[T] = SortSet(t)
}

case class SSetInsert[T <: SymbolicSort](set: SVal[SortSet[T]], values: Set[SVal[T]]) extends SymbolicSet[T] {


  override def typ: SortSet[T] = set.typ
}


case class SSetUnion[T <: SymbolicSort](set1: SVal[SortSet[T]], set2: SVal[SortSet[T]]) extends SymbolicSet[T] {
  override def typ: SortSet[T] = set1.typ
}

case class SSetContains[T <: SymbolicSort](set: SVal[SortSet[T]], value: SVal[T]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


sealed abstract class Quantifier

case class QForall() extends Quantifier {
  override def toString: String = "∀"
}

case class QExists() extends Quantifier {
  override def toString: String = "∃"
}


case class QuantifierExpr(
  quantifier: Quantifier,
  variable: SymbolicVariable[_ <: SymbolicSort],
  body: SVal[SortBoolean]
) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SAggregateExpr[T <: SymbolicSort](
  op: SAggregateOp[T],
  variables: List[SymbolicVariable[_ <: SymbolicSort]],
  filter: SVal[SortBoolean],
  elem: SVal[T]
) extends SVal[T] {
  override def typ: T = op.typ
}

sealed abstract class SAggregateOp[T <: SymbolicSort] {
  def typ: T

}

case class SAggregateSum() extends SAggregateOp[SortInt] {
  override def typ: SortInt = SortInt()
}


case class SCommitted() extends SVal[SortTransactionStatus] {
  override def typ: SortTransactionStatus = SortTransactionStatus()
}

case class SUncommitted() extends SVal[SortTransactionStatus] {
  override def typ: SortTransactionStatus = SortTransactionStatus()
}


case class SNamedVal[T <: SymbolicSort](name: String, value: SVal[T]) extends SVal[T] {
  override def typ: T = value.typ
}

case class SBool(value: Boolean) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SNot(value: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SAnd(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SOr(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SImplies(left: SVal[SortBoolean], right: SVal[SortBoolean]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}


case class SFunctionCall[T <: SymbolicSort](typ: T, func: UninterpretedFunction[T], args: List[SVal[_ <: SymbolicSort]])
  extends SVal[T] {
}

case class SDatatypeValue(inType: SortDatatypeImpl, constructorName: String, values: List[SVal[_ <: SymbolicSort]], dtyp: SortDatatype) extends SVal[SortDatatype] {
  override def typ: SortDatatype = dtyp
}

case class SCallInfo(operationName: String, args: List[SVal[SymbolicSort]]) extends SVal[SortCall] {
  override def typ: SortCall = SortCall()
}

case class SCallInfoNone() extends SVal[SortCall] {
  override def typ: SortCall = SortCall()
}


case class SInvocationInfo(procname: String, args: List[SVal[SortValue]]) extends SVal[SortInvocationInfo] {
  override def typ: SortInvocationInfo = SortInvocationInfo()
}

case class SInvocationInfoNone() extends SVal[SortInvocationInfo] {
  override def typ: SortInvocationInfo = SortInvocationInfo()
}

case class MapDomain[K <: SymbolicSort, V <: SymbolicSort](map: SVal[SortMap[K, SortOption[V]]]) extends SVal[SortSet[K]] {
  override def typ: SortSet[K] = SortSet(map.typ.keySort)
}

case class IsSubsetOf[T <: SymbolicSort](left: SVal[SortSet[T]], right: SVal[SortSet[T]]) extends SVal[SortBoolean] {
  override def typ: SortBoolean = SortBoolean()
}

case class SValOpaque[T <: SymbolicSort](v: Any, typ: T) extends SVal[T] {
  override def toString: String =
  //    if (kind.toString == "UNINTERPRETED_CONSTANT")
    v.toString

  //    else
  //      super.toString
}

case class SBinaryInt(op: SIntOp, left: SVal[SortInt], right: SVal[SortInt]) extends SVal[SortInt] {
  override def typ: SortInt = SortInt()
}

sealed abstract class SIntOp {
  def print: PrettyPrintDoc.Doc = this match {
    case SPlus() => "+"
    case SMinus() =>"-"
    case SMult() =>"*"
    case SDiv() =>"/"
    case SMod() => "%"
  }

}

case class SPlus() extends SIntOp
case class SMinus() extends SIntOp
case class SMult() extends SIntOp
case class SDiv() extends SIntOp
case class SMod() extends SIntOp


