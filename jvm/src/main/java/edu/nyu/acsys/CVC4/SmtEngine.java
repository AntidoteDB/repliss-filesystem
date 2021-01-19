/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class SmtEngine implements SmtEngineI {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected SmtEngine(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(SmtEngine obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    dlRef(emRef);
    emRef = null;
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_SmtEngine(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  // a ref is kept here to keep Java GC from collecting the EM
  // before the SmtEngine
  private Object emRef;
  static final native Object mkRef(Object obj);
  static final native void dlRef(Object obj);

  public SmtEngine(ExprManager em) {
    this(CVC4JNI.new_SmtEngine(ExprManager.getCPtr(em), em), true);
    emRef = mkRef(em); // keep ref to expr manager in SWIG proxy class
  }

  @Override
  public boolean isFullyInited() {
    return CVC4JNI.SmtEngine_isFullyInited(swigCPtr, this);
  }

  @Override
  public void setLogic(String logic) {
    CVC4JNI.SmtEngine_setLogic__SWIG_0(swigCPtr, this, logic);
  }

  @Override
  public void setLogic(LogicInfo logic) {
    CVC4JNI.SmtEngine_setLogic__SWIG_1(swigCPtr, this, LogicInfo.getCPtr(logic), logic);
  }

  @Override
  public LogicInfo getLogicInfo() {
    return new LogicInfo(CVC4JNI.SmtEngine_getLogicInfo(swigCPtr, this), true);
  }

  @Override
  public void setInfo(String key, SExpr value) {
    CVC4JNI.SmtEngine_setInfo(swigCPtr, this, key, SExpr.getCPtr(value), value);
  }

  @Override
  public SExpr getInfo(String key) {
    return new SExpr(CVC4JNI.SmtEngine_getInfo(swigCPtr, this, key), true);
  }

  @Override
  public void setOption(String key, SExpr value) {
    CVC4JNI.SmtEngine_setOption(swigCPtr, this, key, SExpr.getCPtr(value), value);
  }

  @Override
  public void setIsInternalSubsolver() {
    CVC4JNI.SmtEngine_setIsInternalSubsolver(swigCPtr, this);
  }

  @Override
  public void setFilename(String filename) {
    CVC4JNI.SmtEngine_setFilename(swigCPtr, this, filename);
  }

  @Override
  public String getFilename() {
    return CVC4JNI.SmtEngine_getFilename(swigCPtr, this);
  }

  @Override
  public SWIGTYPE_p_CVC4__Model getModel() {
    long cPtr = CVC4JNI.SmtEngine_getModel(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_CVC4__Model(cPtr, false);
  }

  @Override
  public Expr getSepHeapExpr() {
    return new Expr(CVC4JNI.SmtEngine_getSepHeapExpr(swigCPtr, this), true);
  }

  @Override
  public Expr getSepNilExpr() {
    return new Expr(CVC4JNI.SmtEngine_getSepNilExpr(swigCPtr, this), true);
  }

  @Override
  public SExpr getOption(String key) {
    return new SExpr(CVC4JNI.SmtEngine_getOption(swigCPtr, this, key), true);
  }

  @Override
  public void defineFunction(Expr func, vectorExpr formals, Expr formula) {
    CVC4JNI.SmtEngine_defineFunction(swigCPtr, this, Expr.getCPtr(func), func, vectorExpr.getCPtr(formals), formals, Expr.getCPtr(formula), formula);
  }

  @Override
  public boolean isDefinedFunction(Expr func) {
    return CVC4JNI.SmtEngine_isDefinedFunction(swigCPtr, this, Expr.getCPtr(func), func);
  }

  @Override
  public void defineFunctionsRec(vectorExpr funcs, vectorVectorExpr formals, vectorExpr formulas) {
    CVC4JNI.SmtEngine_defineFunctionsRec(swigCPtr, this, vectorExpr.getCPtr(funcs), funcs, vectorVectorExpr.getCPtr(formals), formals, vectorExpr.getCPtr(formulas), formulas);
  }

  @Override
  public void defineFunctionRec(Expr func, vectorExpr formals, Expr formula) {
    CVC4JNI.SmtEngine_defineFunctionRec(swigCPtr, this, Expr.getCPtr(func), func, vectorExpr.getCPtr(formals), formals, Expr.getCPtr(formula), formula);
  }

  @Override
  public Result assertFormula(Expr e, boolean inUnsatCore) {
    return new Result(CVC4JNI.SmtEngine_assertFormula__SWIG_0(swigCPtr, this, Expr.getCPtr(e), e, inUnsatCore), true);
  }

  @Override
  public Result assertFormula(Expr e) {
    return new Result(CVC4JNI.SmtEngine_assertFormula__SWIG_1(swigCPtr, this, Expr.getCPtr(e), e), true);
  }

  @Override
  public Result query(Expr assumption, boolean inUnsatCore) {
    return new Result(CVC4JNI.SmtEngine_query__SWIG_0(swigCPtr, this, Expr.getCPtr(assumption), assumption, inUnsatCore), true);
  }

  @Override
  public Result query(Expr assumption) {
    return new Result(CVC4JNI.SmtEngine_query__SWIG_1(swigCPtr, this, Expr.getCPtr(assumption), assumption), true);
  }

  @Override
  public Result query() {
    return new Result(CVC4JNI.SmtEngine_query__SWIG_2(swigCPtr, this), true);
  }

  @Override
  public Result query(vectorExpr assumptions, boolean inUnsatCore) {
    return new Result(CVC4JNI.SmtEngine_query__SWIG_3(swigCPtr, this, vectorExpr.getCPtr(assumptions), assumptions, inUnsatCore), true);
  }

  @Override
  public Result query(vectorExpr assumptions) {
    return new Result(CVC4JNI.SmtEngine_query__SWIG_4(swigCPtr, this, vectorExpr.getCPtr(assumptions), assumptions), true);
  }

  @Override
  public Result checkSat(Expr assumption, boolean inUnsatCore) {
    return new Result(CVC4JNI.SmtEngine_checkSat__SWIG_0(swigCPtr, this, Expr.getCPtr(assumption), assumption, inUnsatCore), true);
  }

  @Override
  public Result checkSat(Expr assumption) {
    return new Result(CVC4JNI.SmtEngine_checkSat__SWIG_1(swigCPtr, this, Expr.getCPtr(assumption), assumption), true);
  }

  @Override
  public Result checkSat() {
    return new Result(CVC4JNI.SmtEngine_checkSat__SWIG_2(swigCPtr, this), true);
  }

  @Override
  public Result checkSat(vectorExpr assumptions, boolean inUnsatCore) {
    return new Result(CVC4JNI.SmtEngine_checkSat__SWIG_3(swigCPtr, this, vectorExpr.getCPtr(assumptions), assumptions, inUnsatCore), true);
  }

  @Override
  public Result checkSat(vectorExpr assumptions) {
    return new Result(CVC4JNI.SmtEngine_checkSat__SWIG_4(swigCPtr, this, vectorExpr.getCPtr(assumptions), assumptions), true);
  }

  @Override
  public vectorExpr getUnsatAssumptions() {
    return new vectorExpr(CVC4JNI.SmtEngine_getUnsatAssumptions(swigCPtr, this), true);
  }

  @Override
  public void declareSygusVar(String id, Expr var, Type type) {
    CVC4JNI.SmtEngine_declareSygusVar(swigCPtr, this, id, Expr.getCPtr(var), var, Type.getCPtr(type), type);
  }

  @Override
  public void declareSygusPrimedVar(String id, Type type) {
    CVC4JNI.SmtEngine_declareSygusPrimedVar(swigCPtr, this, id, Type.getCPtr(type), type);
  }

  @Override
  public void declareSygusFunctionVar(String id, Expr var, Type type) {
    CVC4JNI.SmtEngine_declareSygusFunctionVar(swigCPtr, this, id, Expr.getCPtr(var), var, Type.getCPtr(type), type);
  }

  @Override
  public void declareSynthFun(String id, Expr func, Type type, boolean isInv, vectorExpr vars) {
    CVC4JNI.SmtEngine_declareSynthFun(swigCPtr, this, id, Expr.getCPtr(func), func, Type.getCPtr(type), type, isInv, vectorExpr.getCPtr(vars), vars);
  }

  @Override
  public void assertSygusConstraint(Expr constraint) {
    CVC4JNI.SmtEngine_assertSygusConstraint(swigCPtr, this, Expr.getCPtr(constraint), constraint);
  }

  @Override
  public void assertSygusInvConstraint(Expr inv, Expr pre, Expr trans, Expr post) {
    CVC4JNI.SmtEngine_assertSygusInvConstraint(swigCPtr, this, Expr.getCPtr(inv), inv, Expr.getCPtr(pre), pre, Expr.getCPtr(trans), trans, Expr.getCPtr(post), post);
  }

  @Override
  public Result checkSynth() {
    return new Result(CVC4JNI.SmtEngine_checkSynth(swigCPtr, this), true);
  }

  @Override
  public Expr simplify(Expr e) {
    return new Expr(CVC4JNI.SmtEngine_simplify(swigCPtr, this, Expr.getCPtr(e), e), true);
  }

  @Override
  public Expr expandDefinitions(Expr e) {
    return new Expr(CVC4JNI.SmtEngine_expandDefinitions(swigCPtr, this, Expr.getCPtr(e), e), true);
  }

  @Override
  public Expr getValue(Expr e) {
    return new Expr(CVC4JNI.SmtEngine_getValue(swigCPtr, this, Expr.getCPtr(e), e), true);
  }

  @Override
  public boolean addToAssignment(Expr e) {
    return CVC4JNI.SmtEngine_addToAssignment(swigCPtr, this, Expr.getCPtr(e), e);
  }

  @Override
  public SWIGTYPE_p_std__vectorT_std__pairT_CVC4__Expr_CVC4__Expr_t_t getAssignment() {
    return new SWIGTYPE_p_std__vectorT_std__pairT_CVC4__Expr_CVC4__Expr_t_t(CVC4JNI.SmtEngine_getAssignment(swigCPtr, this), true);
  }

  @Override
  public Proof getProof() {
    return new Proof(CVC4JNI.SmtEngine_getProof(swigCPtr, this), false);
  }

  @Override
  public void printInstantiations(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.SmtEngine_printInstantiations(swigCPtr, this, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  @Override
  public void printSynthSolution(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.SmtEngine_printSynthSolution(swigCPtr, this, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  @Override
  public void getSynthSolutions(SWIGTYPE_p_std__mapT_CVC4__Expr_CVC4__Expr_t sol_map) {
    CVC4JNI.SmtEngine_getSynthSolutions(swigCPtr, this, SWIGTYPE_p_std__mapT_CVC4__Expr_CVC4__Expr_t.getCPtr(sol_map));
  }

  @Override
  public Expr doQuantifierElimination(Expr e, boolean doFull, boolean strict) {
    return new Expr(CVC4JNI.SmtEngine_doQuantifierElimination__SWIG_0(swigCPtr, this, Expr.getCPtr(e), e, doFull, strict), true);
  }

  @Override
  public Expr doQuantifierElimination(Expr e, boolean doFull) {
    return new Expr(CVC4JNI.SmtEngine_doQuantifierElimination__SWIG_1(swigCPtr, this, Expr.getCPtr(e), e, doFull), true);
  }

  @Override
  public void getInstantiatedQuantifiedFormulas(vectorExpr qs) {
    CVC4JNI.SmtEngine_getInstantiatedQuantifiedFormulas(swigCPtr, this, vectorExpr.getCPtr(qs), qs);
  }

  @Override
  public void getInstantiations(Expr q, vectorExpr insts) {
    CVC4JNI.SmtEngine_getInstantiations(swigCPtr, this, Expr.getCPtr(q), q, vectorExpr.getCPtr(insts), insts);
  }

  @Override
  public void getInstantiationTermVectors(Expr q, vectorVectorExpr tvecs) {
    CVC4JNI.SmtEngine_getInstantiationTermVectors(swigCPtr, this, Expr.getCPtr(q), q, vectorVectorExpr.getCPtr(tvecs), tvecs);
  }

  @Override
  public UnsatCore getUnsatCore() {
    return new UnsatCore(CVC4JNI.SmtEngine_getUnsatCore(swigCPtr, this), true);
  }

  @Override
  public vectorExpr getAssertions() {
    return new vectorExpr(CVC4JNI.SmtEngine_getAssertions(swigCPtr, this), true);
  }

  @Override
  public void push() {
    CVC4JNI.SmtEngine_push(swigCPtr, this);
  }

  @Override
  public void pop() {
    CVC4JNI.SmtEngine_pop(swigCPtr, this);
  }

  @Override
  public void reset() {
    CVC4JNI.SmtEngine_reset(swigCPtr, this);
  }

  @Override
  public void resetAssertions() {
    CVC4JNI.SmtEngine_resetAssertions(swigCPtr, this);
  }

  @Override
  public void interrupt() {
    CVC4JNI.SmtEngine_interrupt(swigCPtr, this);
  }

  @Override
  public void setResourceLimit(long units, boolean cumulative) {
    CVC4JNI.SmtEngine_setResourceLimit__SWIG_0(swigCPtr, this, units, cumulative);
  }

  @Override
  public void setResourceLimit(long units) {
    CVC4JNI.SmtEngine_setResourceLimit__SWIG_1(swigCPtr, this, units);
  }

  @Override
  public void setTimeLimit(long millis, boolean cumulative) {
    CVC4JNI.SmtEngine_setTimeLimit__SWIG_0(swigCPtr, this, millis, cumulative);
  }

  @Override
  public void setTimeLimit(long millis) {
    CVC4JNI.SmtEngine_setTimeLimit__SWIG_1(swigCPtr, this, millis);
  }

  @Override
  public long getResourceUsage() {
    return CVC4JNI.SmtEngine_getResourceUsage(swigCPtr, this);
  }

  @Override
  public long getTimeUsage() {
    return CVC4JNI.SmtEngine_getTimeUsage(swigCPtr, this);
  }

  @Override
  public long getResourceRemaining() {
    return CVC4JNI.SmtEngine_getResourceRemaining(swigCPtr, this);
  }

  @Override
  public long getTimeRemaining() {
    return CVC4JNI.SmtEngine_getTimeRemaining(swigCPtr, this);
  }

  @Override
  public ExprManager getExprManager() {
    long cPtr = CVC4JNI.SmtEngine_getExprManager(swigCPtr, this);
    return (cPtr == 0) ? null : new ExprManager(cPtr, false);
  }

  @Override
  public Statistics getStatistics() {
    return new Statistics(CVC4JNI.SmtEngine_getStatistics(swigCPtr, this), true);
  }

  @Override
  public SExpr getStatistic(String name) {
    return new SExpr(CVC4JNI.SmtEngine_getStatistic(swigCPtr, this, name), true);
  }

  @Override
  public void safeFlushStatistics(int fd) {
    CVC4JNI.SmtEngine_safeFlushStatistics(swigCPtr, this, fd);
  }

  @Override
  public Result getStatusOfLastCommand() {
    return new Result(CVC4JNI.SmtEngine_getStatusOfLastCommand(swigCPtr, this), true);
  }

  @Override
  public void setUserAttribute(String attr, Expr expr, vectorExpr expr_values, String str_value) {
    CVC4JNI.SmtEngine_setUserAttribute(swigCPtr, this, attr, Expr.getCPtr(expr), expr, vectorExpr.getCPtr(expr_values), expr_values, str_value);
  }

  @Override
  public void setPrintFuncInModel(Expr f, boolean p) {
    CVC4JNI.SmtEngine_setPrintFuncInModel(swigCPtr, this, Expr.getCPtr(f), f, p);
  }

  @Override
  public void beforeSearch() {
    CVC4JNI.SmtEngine_beforeSearch(swigCPtr, this);
  }

  @Override
  public SWIGTYPE_p_LemmaChannels channels() {
    long cPtr = CVC4JNI.SmtEngine_channels(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_LemmaChannels(cPtr, false);
  }

  @Override
  public void setReplayStream(ExprStream exprStream) {
    CVC4JNI.SmtEngine_setReplayStream(swigCPtr, this, ExprStream.getCPtr(exprStream), exprStream);
  }

  @Override
  public boolean getExpressionName(Expr e, SWIGTYPE_p_std__string name) {
    return CVC4JNI.SmtEngine_getExpressionName(swigCPtr, this, Expr.getCPtr(e), e, SWIGTYPE_p_std__string.getCPtr(name));
  }

  @Override
  public void setExpressionName(Expr e, String name) {
    CVC4JNI.SmtEngine_setExpressionName(swigCPtr, this, Expr.getCPtr(e), e, name);
  }

}