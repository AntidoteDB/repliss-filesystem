/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class CommandPrintSuccess {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected CommandPrintSuccess(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(CommandPrintSuccess obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_CommandPrintSuccess(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public CommandPrintSuccess(boolean printSuccess) {
    this(CVC4JNI.new_CommandPrintSuccess(printSuccess), true);
  }

  public void applyPrintSuccess(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.CommandPrintSuccess_applyPrintSuccess(swigCPtr, this, edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public static boolean getPrintSuccess(java.io.OutputStream out) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      return CVC4JNI.CommandPrintSuccess_getPrintSuccess(edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout));
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

  public static void setPrintSuccess(java.io.OutputStream out, boolean printSuccess) {
    edu.nyu.acsys.CVC4.JavaOutputStreamAdapter tempout = new edu.nyu.acsys.CVC4.JavaOutputStreamAdapter();
    try {
      CVC4JNI.CommandPrintSuccess_setPrintSuccess(edu.nyu.acsys.CVC4.JavaOutputStreamAdapter.getCPtr(tempout), printSuccess);
    } finally {
    new java.io.PrintStream(out).print(tempout.toString());
    }
  }

}