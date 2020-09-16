package crdtver.symbolic.smt

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}

import crdtver.symbolic._
import crdtver.symbolic.smt.Smt.{Exists, Forall, FuncDef, SmtExpr}
import crdtver.symbolic.smt.Solver._
import crdtver.utils.{ConcurrencyUtils, NativeUtils, ProcessUtils, myMemo}
import edu.nyu.acsys.CVC4.{DatatypeConstructor, _}

/** object for synchronized loading of CVC4 library */
object Cvc4Solver {
  private val lock = new Object
  private var loaded = false

  def loadLibrary(): Unit = {
    lock.synchronized {
      if (!loaded) {
        NativeUtils.loadLibraryFromJar("/native/libcvc4.so")
        NativeUtils.loadLibraryFromJar("/native/libcvc4parser.so")
        NativeUtils.loadLibraryFromJar("/native/libcvc4jni.so")
        loaded = true
      }
    }
  }

  def version(): String = {
    loadLibrary()
    Configuration.getVersionString
  }

}

/**
 *
 * @param checkSatCmd if true, also runs the cvc4 checks on the commandline to see if the results are the same
 */
class Cvc4Solver(
  checkSatCmd: Boolean = false,
  finiteModelFind: Boolean = false
) extends Solver {

  override def toString: String = if (finiteModelFind) "cvc4f" else "cvc4"

  Cvc4Solver.loadLibrary()

  var checkCount: Int = 0

  override def check(constraints: List[Smt.NamedConstraint], options: List[SmtOption] = List(), name: String): CheckRes = {
    checkCount += 1
    val smtLib = SmtLibPrinter.print(constraints)
    val smtLibIn = smtLib.prettyStr(120)
    Files.writeString(Path.of("model", s"temp${checkCount}.smt"), smtLibIn)
    Files.writeString(Path.of("model", s"temp${checkCount}.cvc"), exportConstraints(constraints))


    val instance = new Instance(options)
    val smt = instance.smt
    val solverName = this.toString
    ConcurrencyUtils.newThreadWithInterruptHandler(
      name = s"$solverName-check-$name",
      onInterrupt = () => {
//        println(s"Interrupt $solverName")
        smt.interrupt()
      },
      work = {
        // TODO push pop optimization
        val assertionsWithTranslation: List[(Smt.NamedConstraint, Expr)] =
          for (e <- constraints) yield {
            val expr = instance.translateExpr(e.constraint)(instance.Context())
            try {
              smt.assertFormula(expr)
            } catch {
              case exc: Throwable =>
                throw new RuntimeException(s"Error asserting $expr\n// ${e.description}\n// ${e.constraint}", exc)
            }
            e -> expr
          }
        val res = smt.checkSat()
        res.isSat match {
          case Result.Sat.UNSAT =>
            if (options contains SmtBuildUnsatCore()) {
              val core = smt.getUnsatCore
              val os = new ByteArrayOutputStream()
              core.toStream(os)
              val coreExprs: Set[String] = new String(os.toByteArray).linesIterator.toSet

              val coreAssertions: List[Smt.NamedConstraint] =
                assertionsWithTranslation.filter(x => {
                  val str = s"ASSERT ${x._2.toString};"
                  coreExprs.contains(str)
                }).map(_._1)

              Unsatisfiable(coreAssertions)
            } else {
              // use all assertions as core if not calculated
              Unsatisfiable(constraints)
            }
          case Result.Sat.SAT_UNKNOWN =>
            Unknown()
          case Result.Sat.SAT =>
            // is it really? verify with exported constraints ...
            if (checkSatCmd && !isSatCmd(constraints)) {
              //          println(SmtPrinter.printScala(assertions.reverse))
              throw new RuntimeException("Different results on CMD and API")
            }

            new Satisfiable {

              override def isIncomplete: Boolean = false

              override def getModel: Model = new Model {

                override def toString: String = {
                  "satisfiable model"
                }

                override def eval(expr: SmtExpr, bool: Boolean): SmtExpr = {
                  if (!options.contains(SmtBuildModel())) {
                    throw new Exception("Process was started without SmtBuildModel option.")
                  }

                  if (expr.isConstant)
                    return expr

                  val translated = instance.translateExpr(expr)(instance.Context())
//                  println(s"expr = $expr")
//                  println(s"trans = $translated")
                  val r = smt.getValue(translated)
                  instance.parseExpr(r)
                }

                override protected def getUniverseIntern(typ: Smt.Type): Option[Set[SmtExpr]] =
                // CVC4 API does not provide method for getting the universe
                  None

                override def getConstraints: List[Smt.NamedConstraint] = constraints
              }
            }
        }
      })
  }

  override def exportConstraints(assertions: List[Smt.NamedConstraint], options: List[SmtOption] = List()): String = {
    val instance = new Instance(options)
    instance.exportConstraints(assertions)
  }

  private def isSatCmd(assertions: List[Smt.NamedConstraint]): Boolean = {
    val cvc4in = exportConstraints(assertions)
    val res = ProcessUtils.runCommand(List("cvc4"), cvc4in)
    //    println(s"cvc4.stdout = '${res.stdout}'")
    //    println(s"cvc4.stderr = '${res.stderr}'")
    !res.stdout.contains("unsat")
  }


  private class Instance(options: List[SmtOption]) {
    private var variables: Map[String, Expr] = Map()
    private var datatypes: List[Smt.Datatype] = List()
    private var sortTypes: List[Smt.Sort] = List()

    val emIntern: ExprManager = new ExprManager()
    val em: ExprManagerI = Cvc4Proxy.exprManager(emIntern)
    val smt: SmtEngineI = init()

    def translate(): Unit = {}


    private def init(): SmtEngineI = {
      val smt: SmtEngineI = Cvc4Proxy.smtEngine(emIntern)
      if (options contains SmtBuildModel()) {
        smt.setOption("produce-models", Cvc4Proxy.SExpr(true))
      }
      if (finiteModelFind) {
        smt.setOption("finite-model-find", Cvc4Proxy.SExpr(true))
      }
      if (options contains SmtBuildUnsatCore()) {
        smt.setOption("produce-unsat-cores", Cvc4Proxy.SExpr(true))
      }

      val timeoutMs: Int = options.find(_.isInstanceOf[SmtTimeout]) match {
        case Some(SmtTimeout(d)) =>
          d.toMillis.toInt
        case _ =>
          30000
      }
      smt.setOption("tlimit", Cvc4Proxy.SExpr(timeoutMs))
      //      options.doForFirst {
      //        case ResourceLimit(limit) =>
      //          smt.setOption("rlimit", Cvc4Proxy.SExpr(limit))
      //      }
      smt.setOption("e-matching", Cvc4Proxy.SExpr(true))
      smt.setOption("incremental", Cvc4Proxy.SExpr(true))
      smt.setOption("produce-assertions", Cvc4Proxy.SExpr(true))
      smt.setOption("output-language", Cvc4Proxy.SExpr("cvc4")); // Set the output-language to CVC's
      smt.setOption("default-dag-thresh", Cvc4Proxy.SExpr(0)); //Disable dagifying the output
      smt
    }

    private def toVectorExpr(exprs: Iterable[Expr]): vectorExpr = {
      val r = new vectorExpr()
      for (e <- exprs) {
        r.add(e)
      }
      r
    }

    private def toVectorType(ts: Iterable[Type]): vectorType = {
      val r = new vectorType()
      for (e <- ts) {
        r.add(e)
      }
      r
    }

    def translateType(typ: Smt.Type): Type = typ match {
      case t: Smt.Sort =>
        translateSort(t)
      case dt: Smt.Datatype =>
        translateDatatype(dt)
      case Smt.IntegerType() =>
        em.integerType()
      case Smt.BoolType() =>
        em.booleanType()
      case Smt.ArrayType(keyType, valueType) =>
        em.mkArrayType(translateType(keyType), translateType(valueType))
      case Smt.SetType(elementType) =>
        em.mkSetType(translateType(elementType))
    }

    private val translateSort: Smt.Sort => SortType = new myMemo[Smt.Sort, SortType](t => {
      sortTypes ::= t
      em.mkSort(t.name)
    })

    private val translateUninterpretedFunction: FuncDef => Expr = new myMemo({ f: FuncDef =>
      val funcType = em.mkFunctionType(toVectorType(f.args.map(translateType)), translateType(f.returnType))
      em.mkVar(f.name, funcType)
    })

    private val translateDatatype: Smt.Datatype => DatatypeType = new myMemo[Smt.Datatype, DatatypeType](dt => {
      val rdt = Cvc4Proxy.Datatype(dt.name)
      for (c <- dt.constructors) {
        val rc = Cvc4Proxy.DatatypeConstructor(c.name)
        for (arg <- c.args) {
          Cvc4Proxy.addConstructorArg(rc, arg.name, translateType(arg.typ))
        }
        Cvc4Proxy.addConstructor(rdt, rc)
      }
      datatypes ::= dt
      em.mkDatatypeType(rdt)
    })

    private def getConstructorExpr(dt: Smt.Datatype, constructor: Smt.DatatypeConstructor): Expr = {
      val tdt: Datatype = translateDatatype(dt).getDatatype
      Cvc4Proxy.getConstructor(tdt, constructor.name)
    }


    private def getConstructor(dt: Smt.Datatype, constructor: Smt.DatatypeConstructor): DatatypeConstructor = {
      val tdt: Datatype = translateDatatype(dt).getDatatype
      tdt.get(constructor.name)
    }

    case class Context(
      boundVars: Map[String, Expr] = Map()
    ) {
      def withVariable(variable: Smt.Variable, v: Expr): Context = copy(boundVars = boundVars + (variable.name -> v))

    }

    def translateExpr(e: Smt.SmtExpr)(implicit ctxt: Context): Expr = {
      try {
        e match {
          case node: Smt.SmtExprNode => node match {
            case Smt.Equals(left, right) =>
              em.mkExpr(Kind.EQUAL, translateExpr(left), translateExpr(right))
            case Smt.Not(of) =>
              em.mkExpr(Kind.NOT, translateExpr(of))
            case Smt.ApplyConstructor(dt, constructor, args) =>
              em.mkExpr(Kind.APPLY_CONSTRUCTOR, getConstructorExpr(dt, constructor), toVectorExpr(args.map(translateExpr)))
            case Smt.ApplySelector(dt, constructor, variable, expr) =>
              val selector = Cvc4Proxy.getSelector(getConstructor(dt, constructor), variable.name)
              em.mkExpr(Kind.APPLY_SELECTOR, selector, translateExpr(expr))
            case Smt.IfThenElse(cond, ifTrue, ifFalse) =>
              em.mkExpr(Kind.ITE, translateExpr(cond), translateExpr(ifTrue), translateExpr(ifFalse))
            case Smt.ApplyTester(dt, constructor, expr) =>
              val selector = Cvc4Proxy.getTester(getConstructor(dt, constructor))
              em.mkExpr(Kind.APPLY_TESTER, selector, translateExpr(expr))
            case Smt.MapSelect(map, key) =>
              em.mkExpr(Kind.SELECT, translateExpr(map), translateExpr(key))
            case Smt.ConstantMap(keyType, defaultValue) =>
              em.mkConst(new ArrayStoreAll(translateType(e.calcType).asInstanceOf[ArrayType], translateExpr(defaultValue)))
            case Smt.MapStore(map, key, newValue) =>
              em.mkExpr(Kind.STORE, translateExpr(map), translateExpr(key), translateExpr(newValue))
            case Smt.SetSingleton(value) =>
              em.mkExpr(Kind.SINGLETON, translateExpr(value))
            case Smt.SetInsert(set, vals) =>
              em.mkExpr(Kind.INSERT, toVectorExpr(vals.map(v => translateExpr(v)) ++ List(translateExpr(set))))
            case Smt.Union(left, right) =>
              em.mkExpr(Kind.UNION, translateExpr(left), translateExpr(right))
            case Smt.QuantifierExpr(quantifier: Smt.Quantifier, variable, expr) =>
              val kind = quantifier match {
                case Forall() => Kind.FORALL
                case Exists() => Kind.EXISTS
              }
              val v = em.mkBoundVar(variable.name, translateType(variable.typ))
              val newContext = ctxt.withVariable(variable, v)
              em.mkExpr(kind, em.mkExpr(Kind.BOUND_VAR_LIST, v), translateExpr(expr)(newContext))
            case Smt.And(left, right) =>
              em.mkExpr(Kind.AND, translateExpr(left), translateExpr(right))
            case Smt.Or(left, right) =>
              em.mkExpr(Kind.OR, translateExpr(left), translateExpr(right))
            case Smt.Implies(left, right) =>
              em.mkExpr(Kind.IMPLIES, translateExpr(left), translateExpr(right))
            case Smt.IsSubsetOf(left, right) =>
              em.mkExpr(Kind.SUBSET, translateExpr(left), translateExpr(right))
            case Smt.SetContains(elem, set) =>
              em.mkExpr(Kind.MEMBER, translateExpr(elem), translateExpr(set))
            case Smt.Leq(left, right) =>
              em.mkExpr(Kind.LEQ, translateExpr(left), translateExpr(right))
            case Smt.Lt(left, right) =>
              em.mkExpr(Kind.LT, translateExpr(left), translateExpr(right))
            case Smt.Plus(left, right) =>
              em.mkExpr(Kind.PLUS, translateExpr(left), translateExpr(right))
            case Smt.Minus(left, right) =>
              em.mkExpr(Kind.MINUS, translateExpr(left), translateExpr(right))
            case Smt.Mult(left, right) =>
              em.mkExpr(Kind.MULT, translateExpr(left), translateExpr(right))
            case Smt.Div(left, right) =>
              em.mkExpr(Kind.INTS_DIVISION_TOTAL, translateExpr(left), translateExpr(right))
            case Smt.Mod(left, right) =>
              em.mkExpr(Kind.INTS_MODULUS_TOTAL, translateExpr(left), translateExpr(right))
            case Smt.ApplyFunc(f, args) =>
              em.mkExpr(Kind.APPLY_UF, translateUninterpretedFunction(f), toVectorExpr(args.map(translateExpr)))
            case Smt.Distinct(elems) =>
              if (elems.size < 2)
                em.mkConst(true)
              else
                em.mkExpr(Kind.DISTINCT, toVectorExpr(elems.map(translateExpr)))
          }
          case Smt.Variable(name, typ) =>
            ctxt.boundVars.get(name) match {
              case Some(value) =>
                value
              case None =>
                // must be a global variable
                variables.get(name) match {
                  case Some(value) =>
                    value
                  case None =>
                    val v = em.mkVar(name, translateType(typ))
                    variables += (name -> v)
                    v
                }
            }
          case Smt.Const(b) =>
            em.mkConst(b)
          case Smt.ConstI(i) =>
            em.mkConst(new Rational(i.bigInteger.toString))
          case Smt.EmptySet(valueType) =>
            em.mkConst(Cvc4Proxy.mkEmptySet(em.mkSetType(translateType(valueType))))
          case Smt.OpaqueExpr(kind, expr) =>
            expr.asInstanceOf[Expr]
        }
      } catch {
        case exc: Throwable =>
          throw new java.lang.Exception(s"Error when translating $e", exc)
      }
    }


    var indent = 0

    def debugPrint(str: String): Unit = {

    }

    private def parseType(st: Type): Smt.Type = {
      st match {
        case at: ArrayType =>
          Smt.ArrayType(parseType(at.getIndexType), parseType(at.getConstituentType))
        case t: SetType =>
          Smt.SetType(parseType(t.getElementType))
        case _ =>
          if (st.isSort) {
            sortTypes.find(_.name == st.toString).getOrElse(throw new RuntimeException(s"Sort type $st not found in $sortTypes"))
          } else if (st.isDatatype) {
            datatypes.find(_.name == st.toString).getOrElse(throw new RuntimeException(s"Datatype $st not found in $datatypes"))
          } else if (st.isSet) {
            parseType(st.castToSet())
          } else {
            throw new RuntimeException(s"Unhandled case: $st (${st.getClass})")
          }
      }
    }


    def parseExpr(expr: Expr): SmtExpr = {
      indent += 1
      val kind = expr.getKind

      lazy val children: List[Expr] =
        (for (i <- 0L until expr.getNumChildren) yield expr.getChild(i)).toList

      debugPrint(s"translate $expr")
      //    debugPrint(s"targetType = $t")
      debugPrint(s"kind = $kind")
      //    debugPrint(s"children = $children")

      val result: Smt.SmtExpr = kind match {
        case Kind.STORE =>
          Smt.MapStore(
            parseExpr(children(0)),
            parseExpr(children(1)),
            parseExpr(children(2)))
        case Kind.STORE_ALL =>
          val const = expr.getConstArrayStoreAll
          Smt.ConstantMap(parseType(const.getType.getIndexType), parseExpr(const.getExpr))
        case Kind.SINGLETON =>
          Smt.SetSingleton(parseExpr(children(0)))
        case Kind.EMPTYSET =>
          Smt.EmptySet(parseType(expr.getConstEmptySet.getType.getElementType))
        case Kind.UNION =>
          Smt.Union(parseExpr(children(0)), parseExpr(children(1)))
        case Kind.APPLY_CONSTRUCTOR =>
          val constructorName = expr.getOperator.toString
          val dt = datatypes.find(dt => dt.constructors.exists(c => c.name == constructorName)).getOrElse(throw new RuntimeException(s"Constructor $constructorName not found in $datatypes"))
          val args: List[SmtExpr] = (0L until expr.getNumChildren).map(i => parseExpr(expr.getChild(i))).toList
          Smt.ApplyConstructor(dt, constructorName, args)
        case Kind.UNINTERPRETED_CONSTANT =>
          Smt.OpaqueExpr(parseType(expr.getType), expr)
        //          Smt.Variable(expr.toString, parseType(expr.getType))
        case Kind.CONST_BOOLEAN =>
          Smt.Const(expr.getConstBoolean)
        case Kind.CONST_RATIONAL =>
          Smt.ConstI(BigInt(expr.getConstRational.toString))
        case _ =>
          //debugPrint(s"opaque with kind $kind")
          //Smt.OpaqueExpr(kind, expr)
          throw new Exception(s"cannot parse kind $kind // $expr")
      }
      debugPrint(s"--> $result")

      indent -= 1
      result
    }


    /**
     * export a list of constraints to the CVC4 input language
     */
    def exportConstraints(constraints: List[Smt.NamedConstraint]): String = {
      val r = new StringBuilder()

      def append(o: Any): Unit = {
        r.append(o)
      }

      append(
        """
          |OPTION "finite-model-find" TRUE;
          |OPTION "produce-models" TRUE;
        """.stripMargin)

      val constraints2: List[(Smt.NamedConstraint, Expr)] =
        for (c <- constraints) yield {
          c -> translateExpr(c.constraint)(Context())
        }

      for (st: Smt.Sort <- sortTypes.reverse) {
        append(s"${st.name}: TYPE;\n")
      }
      for (dt: Smt.Datatype <- datatypes.reverse) {
        append(s"DATATYPE ${dt.name} = \n")
        for ((c, i) <- dt.constructors.zipWithIndex) {
          if (i > 0) {
            append(" | ")
          } else {
            append("   ")
          }
          append(s"${c.name}")
          if (c.args.nonEmpty) {
            append("(")
            for ((arg, j) <- c.args.zipWithIndex) {
              if (j > 0) {
                append(", ")
              }
              append(s"${arg.name}: ${translateType(arg.typ)}")
            }
            append(")")
          }
          append("\n")
        }
        append("END;\n")
      }


      for ((v, t) <- variables) {
        append(s"$v: ${t.getType};\n")
      }

      for ((named, a) <- constraints2.reverse) {
        append("\n")
        append(s"% ${named.description.replaceAll("\n", "\n% ")}\n")
        //        append(s"% ${SmtPrinter.printScala(named.constraint, SmtPrinter.PrintContext()).pretty(120).layout().replaceAll("\n", "\n% ")}\n")
        //
        //      val os = new ByteArrayOutputStream()
        //
        //      a.toStream(os, Int.MaxValue, true, 1000L, OutputLanguage.OUTPUT_LANG_TPTP)
        //
        //      val r = os.toString(StandardCharsets.UTF_8)
        //
        //      append(s"%----\n% ${r.replaceAll("\n", "\n% ")}\n")

        append(s"ASSERT $a;\n")
      }


      append("CHECKSAT;\n")
      //      append("COUNTERMODEL;\n")
      //      append("COUNTEREXAMPLE;\n")
      append("\n")
      r.toString()
    }
  }


}

