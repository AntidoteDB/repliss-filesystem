/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class vectorCommandPtr {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected vectorCommandPtr(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(vectorCommandPtr obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

/*   protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_vectorCommandPtr(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public vectorCommandPtr() {
    this(CVC4JNI.new_vectorCommandPtr__SWIG_0(), true);
  }

  public vectorCommandPtr(long n) {
    this(CVC4JNI.new_vectorCommandPtr__SWIG_1(n), true);
  }

  public long size() {
    return CVC4JNI.vectorCommandPtr_size(swigCPtr, this);
  }

  public long capacity() {
    return CVC4JNI.vectorCommandPtr_capacity(swigCPtr, this);
  }

  public void reserve(long n) {
    CVC4JNI.vectorCommandPtr_reserve(swigCPtr, this, n);
  }

  public boolean isEmpty() {
    return CVC4JNI.vectorCommandPtr_isEmpty(swigCPtr, this);
  }

  public void clear() {
    CVC4JNI.vectorCommandPtr_clear(swigCPtr, this);
  }

  public void add(Command x) {
    CVC4JNI.vectorCommandPtr_add(swigCPtr, this, Command.getCPtr(x), x);
  }

  public Command get(int i) {
    long cPtr = CVC4JNI.vectorCommandPtr_get(swigCPtr, this, i);
    return (cPtr == 0) ? null : new Command(cPtr, false);
  }

  public void set(int i, Command val) {
    CVC4JNI.vectorCommandPtr_set(swigCPtr, this, i, Command.getCPtr(val), val);
  }

}