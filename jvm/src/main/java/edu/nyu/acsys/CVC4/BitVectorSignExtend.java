/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class BitVectorSignExtend {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected BitVectorSignExtend(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(BitVectorSignExtend obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_BitVectorSignExtend(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public void setSignExtendAmount(long value) {
    CVC4JNI.BitVectorSignExtend_signExtendAmount_set(swigCPtr, this, value);
  }

  public long getSignExtendAmount() {
    return CVC4JNI.BitVectorSignExtend_signExtendAmount_get(swigCPtr, this);
  }

  public BitVectorSignExtend(long signExtendAmount) {
    this(CVC4JNI.new_BitVectorSignExtend(signExtendAmount), true);
  }

  public long toUnsigned() {
    return CVC4JNI.BitVectorSignExtend_toUnsigned(swigCPtr, this);
  }

}