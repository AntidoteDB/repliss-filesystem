/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class RecordUpdateHashFunction {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected RecordUpdateHashFunction(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(RecordUpdateHashFunction obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_RecordUpdateHashFunction(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public long apply(RecordUpdate t) {
    return CVC4JNI.RecordUpdateHashFunction_apply(swigCPtr, this, RecordUpdate.getCPtr(t), t);
  }

  public RecordUpdateHashFunction() {
    this(CVC4JNI.new_RecordUpdateHashFunction(), true);
  }

}