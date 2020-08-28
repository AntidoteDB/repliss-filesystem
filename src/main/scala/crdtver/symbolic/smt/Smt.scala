package crdtver.symbolic.smt

import crdtver.language.TypedAst.IntType
import crdtver.utils.PrettyPrintDoc.Doc

/**
 *
 */
object Smt {


  sealed abstract class SmtExpr {
    def isConstant: Boolean = this match {
      case node: SmtExprNode =>
        node match {
          case Equals(left, right) =>
            false
          case Not(of) =>
            false
          case ApplyConstructor(dt, constructor, args) =>
            args.forall(_.isConstant)
          case ApplySelector(dt, constructor, variable, expr) =>
            false
          case ApplyFunc(f, args) =>
            false
          case IfThenElse(cond, ifTrue, ifFalse) =>
            false
          case ApplyTester(dt, constructor, expr) =>
            false
          case MapSelect(map, key) =>
            false
          case ConstantMap(keyType, defaultValue) =>
            defaultValue.isConstant
          case MapStore(map, key, newValue) =>
            map.isConstant && key.isConstant && newValue.isConstant
          case SetSingleton(value) =>
            value.isConstant
          case SetInsert(set, values) =>
            set.isConstant && values.forall(_.isConstant)
          case Union(left, right) =>
            false
          case QuantifierExpr(quantifier, variable, expr) =>
            false
          case And(left, right) =>
            false
          case Or(left, right) =>
            false
          case Implies(left, right) =>
            false
          case IsSubsetOf(left, right) =>
            false
          case SetContains(element, set) =>
            false
          case Leq(left, right) =>
            false
          case Lt(left, right) =>
            false
          case Distinct(elems) =>
            false
          case _: Div | _: Minus | _: Mod | _: Mult | _: Plus  =>
            false
        }
      case Variable(name, typ) => false
      case Const(b) => true
      case ConstI(i) => true
      case EmptySet(valueType) => true
      case OpaqueExpr(typ, expr) => false
    }

    def subst(vars: Map[Variable, SmtExpr]): SmtExpr = this match {
      case Equals(left, right) =>
        Equals(left.subst(vars), right.subst(vars))
      case Not(of) =>
        Not(of.subst(vars))
      case ApplyConstructor(dt, constructor, args) =>
        ApplyConstructor(dt, constructor, args.map(_.subst(vars)))
      case ApplySelector(dt, constructor, variable, expr) =>
        ApplySelector(dt, constructor, variable, expr.subst(vars))
      case ApplyFunc(f, args) =>
        ApplyFunc(f, args.map(_.subst(vars)))
      case IfThenElse(cond, ifTrue, ifFalse) =>
        IfThenElse(cond.subst(vars), ifTrue.subst(vars), ifFalse.subst(vars))
      case ApplyTester(dt, constructor, expr) =>
        ApplyTester(dt, constructor, expr.subst(vars))
      case MapSelect(map, key) =>
        MapSelect(map.subst(vars), key.subst(vars))
      case ConstantMap(keyType, defaultValue) =>
        ConstantMap(keyType, defaultValue.subst(vars))
      case MapStore(map, key, newValue) =>
        MapStore(map.subst(vars), key.subst(vars), newValue.subst(vars))
      case SetSingleton(value) =>
        SetSingleton(value.subst(vars))
      case SetInsert(set, values) =>
        SetInsert(set.subst(vars), values.map(_.subst(vars)))
      case Union(left, right) =>
        Union(left.subst(vars), right.subst(vars))
      case QuantifierExpr(quantifier, variable, expr) =>
        QuantifierExpr(quantifier, variable, expr.subst(vars.removed(variable)))
      case And(left, right) =>
        And(left.subst(vars), right.subst(vars))
      case Or(left, right) =>
        Or(left.subst(vars), right.subst(vars))
      case Implies(left, right) =>
        Implies(left.subst(vars), right.subst(vars))
      case IsSubsetOf(left, right) =>
        IsSubsetOf(left.subst(vars), right.subst(vars))
      case SetContains(element, set) =>
        SetContains(element.subst(vars), set.subst(vars))
      case Leq(left, right) =>
        Leq(left.subst(vars), right.subst(vars))
      case Lt(left, right) =>
        Lt(left.subst(vars), right.subst(vars))
      case Plus(left, right) =>
        Plus(left.subst(vars), right.subst(vars))
      case Minus(left, right) =>
        Minus(left.subst(vars), right.subst(vars))
      case Mult(left, right) =>
        Mult(left.subst(vars), right.subst(vars))
      case Div(left, right) =>
        Div(left.subst(vars), right.subst(vars))
      case Mod(left, right) =>
        Mod(left.subst(vars), right.subst(vars))
      case Distinct(elems) =>
        Distinct(elems.map(_.subst(vars)))
      case v: Variable =>
        vars.getOrElse(v, v)
      case Const(b) =>
        Const(b)
      case ConstI(i) =>
        ConstI(i)
      case EmptySet(valueType) =>
        EmptySet(valueType)
      case OpaqueExpr(typ, expr) =>
        OpaqueExpr(typ, expr)
    }

    def containsQuantifiers: Boolean = this match {
      case _: QuantifierExpr =>
        true
      case node: SmtExprNode =>
        node.children.exists(_.containsQuantifiers)
      case _ =>
        false
    }

    def calcType: Type = this match {
      case node: SmtExprNode => node match {
        case Equals(left, right) => BoolType()
        case Not(of) => BoolType()
        case ApplyConstructor(dt, constructor, args) => dt
        case ApplySelector(dt, constructor, variable, expr) => variable.typ
        case IfThenElse(cond, ifTrue, ifFalse) => ifTrue.calcType
        case ApplyTester(dt, constructor, expr) => BoolType()
        case MapSelect(map, key) => map.calcType.asInstanceOf[ArrayType].valueType
        case ConstantMap(keyType, defaultValue) => ArrayType(keyType, defaultValue.calcType)
        case MapStore(map, key, newValue) => map.calcType
        case SetSingleton(value) => SetType(value.calcType)
        case SetInsert(set, values) => set.calcType
        case Union(left, right) => left.calcType
        case QuantifierExpr(quantifier, variable, expr) => BoolType()
        case And(left, right) => BoolType()
        case Or(left, right) => BoolType()
        case Implies(left, right) => BoolType()
        case IsSubsetOf(left, right) => BoolType()
        case SetContains(element, set) => BoolType()
        case Leq(left, right) => BoolType()
        case Lt(left, right) => BoolType()
        case ApplyFunc(f, args) => f.returnType
        case Distinct(elems) =>
          BoolType()
        case Div(_, _) | Minus(_, _) | Mod(_, _) | Mult(_, _) | Plus(_, _) =>
          IntegerType()
      }
      case Variable(name, typ) => typ
      case Const(b) => BoolType()
      case ConstI(i) => IntegerType()
      case EmptySet(valueType) =>
        SetType(valueType)
      case OpaqueExpr(typ, expr) =>
        typ
    }

    def children: Iterable[SmtExpr] = List()

    def prettyPrint: Doc =
      SmtLibPrinter.printExpr(this, SmtLibPrinter.PrintContext())

    override def toString: String =
      SmtPrinter.printScala(this, SmtPrinter.PrintContext()).prettyStr(120)
  }

  sealed abstract class SmtExprNode(subExpressions: SmtExpr*) extends SmtExpr {
    override def children: Iterable[SmtExpr] = subExpressions
  }


  //  case class SmtApply(func: SmtFunction, args: List[SmtExpr]) extends SmtExpr
  //
  //  def SmtApply(func: SmtFunction, args: SmtExpr*): SmtApply = SmtApply(func, args.toList)


  sealed abstract class SmtFunction

  sealed abstract class Type {
    def typeName(): String = this match {
      case Smt.Sort(name) => name
      case Datatype(name, _) => name
      case Smt.IntegerType() => "integer"
      case Smt.BoolType() => "bool"
      case Smt.ArrayType(keyType, valueType) => s"array_from_${keyType.typeName()}_to_${valueType.typeName()}"
      case Smt.SetType(elementType) => s"set_of_${elementType.typeName()}"
    }

  }


  // uninterpreted sort
  case class Sort(name: String) extends Type

  case class Datatype(
    name: String,
    constructors: List[DatatypeConstructor]
  ) extends Type {
    def getConstructor(constructorName: String): DatatypeConstructor =
      constructors.find(_.name == constructorName).getOrElse(throw new RuntimeException(s"Constructor $constructorName not found in $this"))

  }

  case class DatatypeConstructor(name: String, args: List[Variable])

  /** uninterpreted function */
  case class FuncDef(name: String, args: List[Type], returnType: Type)

  case class Variable(name: String, typ: Type) extends SmtExpr

  case class IntegerType() extends Type

  case class BoolType() extends Type

  case class ArrayType(keyType: Type, valueType: Type) extends Type

  case class SetType(elementType: Type) extends Type

  case class Const(b: Boolean) extends SmtExpr

  case class ConstI(i: BigInt) extends SmtExpr


  case class Equals(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right) {
    override def children: Iterable[SmtExpr] = List(left, right)
  }

  case class Not(of: SmtExpr) extends SmtExprNode(of)

  case class ApplyConstructor(dt: Datatype, constructor: DatatypeConstructor, args: List[SmtExpr]) extends SmtExprNode(args: _*) {
    require(dt.constructors.contains(constructor))
    require(args.length == constructor.args.length)

    override def toString: String = s"${dt.name}.${constructor.name}(${args.mkString(", ")})"
  }

  def ApplyConstructor(dt: Datatype, constructor: DatatypeConstructor, args: SmtExpr*): ApplyConstructor =
    ApplyConstructor(dt, constructor, args.toList)

  def ApplyConstructor(dt: Datatype, constructor: String, args: SmtExpr*): ApplyConstructor =
    ApplyConstructor(dt, dt.getConstructor(constructor), args.toList)

  def ApplyConstructor(dt: Datatype, constructor: String, args: List[SmtExpr]): ApplyConstructor =
    ApplyConstructor(dt, dt.getConstructor(constructor), args)

  case class ApplySelector(dt: Datatype, constructor: DatatypeConstructor, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
    require(constructor.args.contains(variable))
  }

  case class ApplyFunc(f: FuncDef, args: List[SmtExpr]) extends SmtExprNode(args: _*) {
    require(args.length == f.args.length)

    override def toString: String = s"${f.name}(${args.mkString(", ")})"
  }


  case class IfThenElse(cond: SmtExpr, ifTrue: SmtExpr, ifFalse: SmtExpr) extends SmtExprNode(cond, ifTrue, ifFalse)


  case class ApplyTester(dt: Datatype, constructor: DatatypeConstructor, expr: SmtExpr) extends SmtExprNode(expr) {
    require(dt.constructors.contains(constructor))
  }

  case class MapSelect(map: SmtExpr, key: SmtExpr) extends SmtExprNode(map, key)

  case class ConstantMap(keyType: Type, defaultValue: SmtExpr) extends SmtExprNode(defaultValue)

  case class MapStore(map: SmtExpr, key: SmtExpr, newValue: SmtExpr) extends SmtExprNode(map, key, newValue) {
    map.calcType match {
      case ArrayType(keyType, valueType) =>
        require(keyType == key.calcType, s"Key must be of type $keyType but found ${key.calcType}\nin $this")
        require(valueType == newValue.calcType, s"Value must be of type $valueType but found ${newValue.calcType}\nin $this")
      case _ => require(false, s"Expected Array type but found $map")
    }
  }

  case class SetSingleton(value: SmtExpr) extends SmtExprNode(value)

  case class SetInsert(set: SmtExpr, values: List[SmtExpr]) extends SmtExprNode(set :: values: _*)

  case class EmptySet(valueType: Type) extends SmtExpr

  case class Union(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  sealed abstract class Quantifier

  case class Forall() extends Quantifier

  case class Exists() extends Quantifier

  case class QuantifierExpr(quantifier: Quantifier, variable: Variable, expr: SmtExpr) extends SmtExprNode(expr)

  case class And(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Or(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Implies(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class IsSubsetOf(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class SetContains(element: SmtExpr, set: SmtExpr) extends SmtExprNode(element, set)

  case class Leq(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Lt(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Plus(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Minus(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Mult(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Div(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)

  case class Mod(left: SmtExpr, right: SmtExpr) extends SmtExprNode(left, right)


  case class Distinct(elems: List[SmtExpr]) extends SmtExprNode {
    override def children: Iterable[SmtExpr] = elems
  }

  /** E.g. Uninterpreted constant produced by the solver */
  case class OpaqueExpr(typ: Type, expr: Any) extends SmtExpr

  /** priority, where 0 must be included and lower values are included first */
  case class NamedConstraint(description: String, priority: Int, constraint: SmtExpr)

}
