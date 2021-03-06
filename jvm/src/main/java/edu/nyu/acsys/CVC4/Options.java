/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class Options {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected Options(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(Options obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_Options(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  static public class OptionsScope {
    private transient long swigCPtr;
    protected transient boolean swigCMemOwn;
  
    protected OptionsScope(long cPtr, boolean cMemoryOwn) {
      swigCMemOwn = cMemoryOwn;
      swigCPtr = cPtr;
    }
  
    protected static long getCPtr(OptionsScope obj) {
      return (obj == null) ? 0 : obj.swigCPtr;
    }
  
    /* protected void finalize() {
      delete();
    } */
  
    public synchronized void delete() {
      if (swigCPtr != 0) {
        if (swigCMemOwn) {
          swigCMemOwn = false;
          CVC4JNI.delete_Options_OptionsScope(swigCPtr);
        }
        swigCPtr = 0;
      }
    }
  
    public OptionsScope(Options newOptions) {
      this(CVC4JNI.new_Options_OptionsScope(Options.getCPtr(newOptions), newOptions), true);
    }
  
  }

  public static boolean isCurrentNull() {
    return CVC4JNI.Options_isCurrentNull();
  }

  public static Options current() {
    long cPtr = CVC4JNI.Options_current();
    return (cPtr == 0) ? null : new Options(cPtr, false);
  }

  public Options() {
    this(CVC4JNI.new_Options(), true);
  }

  public void copyValues(Options options) {
    CVC4JNI.Options_copyValues(swigCPtr, this, Options.getCPtr(options), options);
  }

  public void setOption(String key, String optionarg) {
    CVC4JNI.Options_setOption(swigCPtr, this, key, optionarg);
  }

  public String getOption(String key) {
    return CVC4JNI.Options_getOption(swigCPtr, this, key);
  }

  public InputLanguage getInputLanguage() {
    return InputLanguage.swigToEnum(CVC4JNI.Options_getInputLanguage(swigCPtr, this));
  }

  public SWIGTYPE_p_InstFormatMode getInstFormatMode() {
    return new SWIGTYPE_p_InstFormatMode(CVC4JNI.Options_getInstFormatMode(swigCPtr, this), true);
  }

  public OutputLanguage getOutputLanguage() {
    return OutputLanguage.swigToEnum(CVC4JNI.Options_getOutputLanguage(swigCPtr, this));
  }

  public boolean getCheckProofs() {
    return CVC4JNI.Options_getCheckProofs(swigCPtr, this);
  }

  public boolean getContinuedExecution() {
    return CVC4JNI.Options_getContinuedExecution(swigCPtr, this);
  }

  public boolean getDumpInstantiations() {
    return CVC4JNI.Options_getDumpInstantiations(swigCPtr, this);
  }

  public boolean getDumpModels() {
    return CVC4JNI.Options_getDumpModels(swigCPtr, this);
  }

  public boolean getDumpProofs() {
    return CVC4JNI.Options_getDumpProofs(swigCPtr, this);
  }

  public boolean getDumpSynth() {
    return CVC4JNI.Options_getDumpSynth(swigCPtr, this);
  }

  public boolean getDumpUnsatCores() {
    return CVC4JNI.Options_getDumpUnsatCores(swigCPtr, this);
  }

  public boolean getEarlyExit() {
    return CVC4JNI.Options_getEarlyExit(swigCPtr, this);
  }

  public boolean getFallbackSequential() {
    return CVC4JNI.Options_getFallbackSequential(swigCPtr, this);
  }

  public boolean getFilesystemAccess() {
    return CVC4JNI.Options_getFilesystemAccess(swigCPtr, this);
  }

  public boolean getForceNoLimitCpuWhileDump() {
    return CVC4JNI.Options_getForceNoLimitCpuWhileDump(swigCPtr, this);
  }

  public boolean getHelp() {
    return CVC4JNI.Options_getHelp(swigCPtr, this);
  }

  public boolean getIncrementalParallel() {
    return CVC4JNI.Options_getIncrementalParallel(swigCPtr, this);
  }

  public boolean getIncrementalSolving() {
    return CVC4JNI.Options_getIncrementalSolving(swigCPtr, this);
  }

  public boolean getInteractive() {
    return CVC4JNI.Options_getInteractive(swigCPtr, this);
  }

  public boolean getInteractivePrompt() {
    return CVC4JNI.Options_getInteractivePrompt(swigCPtr, this);
  }

  public boolean getLanguageHelp() {
    return CVC4JNI.Options_getLanguageHelp(swigCPtr, this);
  }

  public boolean getMemoryMap() {
    return CVC4JNI.Options_getMemoryMap(swigCPtr, this);
  }

  public boolean getParseOnly() {
    return CVC4JNI.Options_getParseOnly(swigCPtr, this);
  }

  public boolean getProduceModels() {
    return CVC4JNI.Options_getProduceModels(swigCPtr, this);
  }

  public boolean getProof() {
    return CVC4JNI.Options_getProof(swigCPtr, this);
  }

  public boolean getSegvSpin() {
    return CVC4JNI.Options_getSegvSpin(swigCPtr, this);
  }

  public boolean getSemanticChecks() {
    return CVC4JNI.Options_getSemanticChecks(swigCPtr, this);
  }

  public boolean getStatistics() {
    return CVC4JNI.Options_getStatistics(swigCPtr, this);
  }

  public boolean getStatsEveryQuery() {
    return CVC4JNI.Options_getStatsEveryQuery(swigCPtr, this);
  }

  public boolean getStatsHideZeros() {
    return CVC4JNI.Options_getStatsHideZeros(swigCPtr, this);
  }

  public boolean getStrictParsing() {
    return CVC4JNI.Options_getStrictParsing(swigCPtr, this);
  }

  public int getTearDownIncremental() {
    return CVC4JNI.Options_getTearDownIncremental(swigCPtr, this);
  }

  public boolean getVersion() {
    return CVC4JNI.Options_getVersion(swigCPtr, this);
  }

  public boolean getWaitToJoin() {
    return CVC4JNI.Options_getWaitToJoin(swigCPtr, this);
  }

  public String getForceLogicString() {
    return CVC4JNI.Options_getForceLogicString(swigCPtr, this);
  }

  public vectorString getThreadArgv() {
    return new vectorString(CVC4JNI.Options_getThreadArgv(swigCPtr, this), false);
  }

  public int getSharingFilterByLength() {
    return CVC4JNI.Options_getSharingFilterByLength(swigCPtr, this);
  }

  public int getThreadId() {
    return CVC4JNI.Options_getThreadId(swigCPtr, this);
  }

  public int getVerbosity() {
    return CVC4JNI.Options_getVerbosity(swigCPtr, this);
  }

  public SWIGTYPE_p_std__istream getIn() {
    long cPtr = CVC4JNI.Options_getIn(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_std__istream(cPtr, false);
  }

  public SWIGTYPE_p_std__ostream getErr() {
    long cPtr = CVC4JNI.Options_getErr(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_std__ostream(cPtr, false);
  }

  public SWIGTYPE_p_std__ostream getOut() {
    long cPtr = CVC4JNI.Options_getOut(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_std__ostream(cPtr, false);
  }

  public SWIGTYPE_p_std__ostream getOutConst() {
    long cPtr = CVC4JNI.Options_getOutConst(swigCPtr, this);
    return (cPtr == 0) ? null : new SWIGTYPE_p_std__ostream(cPtr, false);
  }

  public String getBinaryName() {
    return CVC4JNI.Options_getBinaryName(swigCPtr, this);
  }

  public String getReplayInputFilename() {
    return CVC4JNI.Options_getReplayInputFilename(swigCPtr, this);
  }

  public long getParseStep() {
    return CVC4JNI.Options_getParseStep(swigCPtr, this);
  }

  public long getThreadStackSize() {
    return CVC4JNI.Options_getThreadStackSize(swigCPtr, this);
  }

  public long getThreads() {
    return CVC4JNI.Options_getThreads(swigCPtr, this);
  }

  public void setInputLanguage(InputLanguage arg0) {
    CVC4JNI.Options_setInputLanguage(swigCPtr, this, arg0.swigValue());
  }

  public void setInteractive(boolean arg0) {
    CVC4JNI.Options_setInteractive(swigCPtr, this, arg0);
  }

  public void setOut(SWIGTYPE_p_std__ostream arg0) {
    CVC4JNI.Options_setOut(swigCPtr, this, SWIGTYPE_p_std__ostream.getCPtr(arg0));
  }

  public void setOutputLanguage(OutputLanguage arg0) {
    CVC4JNI.Options_setOutputLanguage(swigCPtr, this, arg0.swigValue());
  }

  public void setSharingFilterByLength(int length) {
    CVC4JNI.Options_setSharingFilterByLength(swigCPtr, this, length);
  }

  public void setThreadId(int arg0) {
    CVC4JNI.Options_setThreadId(swigCPtr, this, arg0);
  }

  public boolean wasSetByUserCeGuidedInst() {
    return CVC4JNI.Options_wasSetByUserCeGuidedInst(swigCPtr, this);
  }

  public boolean wasSetByUserDumpSynth() {
    return CVC4JNI.Options_wasSetByUserDumpSynth(swigCPtr, this);
  }

  public boolean wasSetByUserEarlyExit() {
    return CVC4JNI.Options_wasSetByUserEarlyExit(swigCPtr, this);
  }

  public boolean wasSetByUserForceLogicString() {
    return CVC4JNI.Options_wasSetByUserForceLogicString(swigCPtr, this);
  }

  public boolean wasSetByUserIncrementalSolving() {
    return CVC4JNI.Options_wasSetByUserIncrementalSolving(swigCPtr, this);
  }

  public boolean wasSetByUserInteractive() {
    return CVC4JNI.Options_wasSetByUserInteractive(swigCPtr, this);
  }

  public boolean wasSetByUserThreadStackSize() {
    return CVC4JNI.Options_wasSetByUserThreadStackSize(swigCPtr, this);
  }

  public boolean wasSetByUserThreads() {
    return CVC4JNI.Options_wasSetByUserThreads(swigCPtr, this);
  }

  public static int currentGetSharingFilterByLength() {
    return CVC4JNI.Options_currentGetSharingFilterByLength();
  }

  public static int currentGetThreadId() {
    return CVC4JNI.Options_currentGetThreadId();
  }

  public static SWIGTYPE_p_std__ostream currentGetOut() {
    long cPtr = CVC4JNI.Options_currentGetOut();
    return (cPtr == 0) ? null : new SWIGTYPE_p_std__ostream(cPtr, false);
  }

  public String getDescription() {
    return CVC4JNI.Options_getDescription(swigCPtr, this);
  }

  public static void printUsage(String msg, java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.Options_printUsage(msg, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public static void printShortUsage(String msg, java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.Options_printShortUsage(msg, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public static void printLanguageHelp(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.Options_printLanguageHelp(edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public static String suggestCommandLineOptions(String optionName) {
    return CVC4JNI.Options_suggestCommandLineOptions(optionName);
  }

  public static vectorString suggestSmtOptions(String optionName) {
    return new vectorString(CVC4JNI.Options_suggestSmtOptions(optionName), true);
  }

  public static vectorString parseOptions(Options options, int argc, String[] argv) {
    return new vectorString(CVC4JNI.Options_parseOptions(Options.getCPtr(options), options, argc, argv), true);
  }

  public SWIGTYPE_p_std__vectorT_std__vectorT_std__string_t_t getOptions() {
    return new SWIGTYPE_p_std__vectorT_std__vectorT_std__string_t_t(CVC4JNI.Options_getOptions(swigCPtr, this), true);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerBeforeSearchListener(SWIGTYPE_p_Listener listener) {
    long cPtr = CVC4JNI.Options_registerBeforeSearchListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener));
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerForceLogicListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerForceLogicListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerTlimitListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerTlimitListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerTlimitPerListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerTlimitPerListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerRlimitListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerRlimitListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerRlimitPerListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerRlimitPerListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerUseTheoryListListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerUseTheoryListListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetDefaultExprDepthListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetDefaultExprDepthListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetDefaultExprDagListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetDefaultExprDagListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetPrintExprTypesListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetPrintExprTypesListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetDumpModeListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetDumpModeListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetPrintSuccessListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetPrintSuccessListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerDumpToFileNameListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerDumpToFileNameListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetRegularOutputChannelListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetRegularOutputChannelListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetDiagnosticOutputChannelListener(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetDiagnosticOutputChannelListener(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public SWIGTYPE_p_ListenerCollection__Registration registerSetReplayLogFilename(SWIGTYPE_p_Listener listener, boolean notifyIfSet) {
    long cPtr = CVC4JNI.Options_registerSetReplayLogFilename(swigCPtr, this, SWIGTYPE_p_Listener.getCPtr(listener), notifyIfSet);
    return (cPtr == 0) ? null : new SWIGTYPE_p_ListenerCollection__Registration(cPtr, false);
  }

  public void flushErr() {
    CVC4JNI.Options_flushErr(swigCPtr, this);
  }

  public void flushOut() {
    CVC4JNI.Options_flushOut(swigCPtr, this);
  }

}
