/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.12
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */

package edu.nyu.acsys.CVC4;

public class CVC4String {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected CVC4String(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(CVC4String obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  /* protected void finalize() {
    delete();
  } */

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        CVC4JNI.delete_CVC4String(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public static long start_code() {
    return CVC4JNI.CVC4String_start_code();
  }

  public static long num_codes() {
    return CVC4JNI.CVC4String_num_codes();
  }

  public static long convertCharToUnsignedInt(short c) {
    return CVC4JNI.CVC4String_convertCharToUnsignedInt(c);
  }

  public static short convertUnsignedIntToChar(long i) {
    return CVC4JNI.CVC4String_convertUnsignedIntToChar(i);
  }

  public static boolean isPrintable(long i) {
    return CVC4JNI.CVC4String_isPrintable(i);
  }

  public static long convertCodeToUnsignedInt(long c) {
    return CVC4JNI.CVC4String_convertCodeToUnsignedInt(c);
  }

  public static long convertUnsignedIntToCode(long i) {
    return CVC4JNI.CVC4String_convertUnsignedIntToCode(i);
  }

  public CVC4String() {
    this(CVC4JNI.new_CVC4String__SWIG_0(), true);
  }

  public CVC4String(String s, boolean useEscSequences) {
    this(CVC4JNI.new_CVC4String__SWIG_1(s, useEscSequences), true);
  }

  public CVC4String(String s) {
    this(CVC4JNI.new_CVC4String__SWIG_3(s), true);
  }

  public CVC4String(vectorUnsignedInt s) {
    this(CVC4JNI.new_CVC4String__SWIG_4(vectorUnsignedInt.getCPtr(s), s), true);
  }

  public CVC4String assign(CVC4String y) {
    return new CVC4String(CVC4JNI.CVC4String_assign(swigCPtr, this, CVC4String.getCPtr(y), y), false);
  }

  public CVC4String concat(CVC4String other) {
    return new CVC4String(CVC4JNI.CVC4String_concat(swigCPtr, this, CVC4String.getCPtr(other), other), true);
  }

  public boolean equals(CVC4String y) {
    return CVC4JNI.CVC4String_equals(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean less(CVC4String y) {
    return CVC4JNI.CVC4String_less(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean greater(CVC4String y) {
    return CVC4JNI.CVC4String_greater(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean lessEqual(CVC4String y) {
    return CVC4JNI.CVC4String_lessEqual(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean greaterEqual(CVC4String y) {
    return CVC4JNI.CVC4String_greaterEqual(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean strncmp(CVC4String y, long np) {
    return CVC4JNI.CVC4String_strncmp(swigCPtr, this, CVC4String.getCPtr(y), y, np);
  }

  public boolean rstrncmp(CVC4String y, long np) {
    return CVC4JNI.CVC4String_rstrncmp(swigCPtr, this, CVC4String.getCPtr(y), y, np);
  }

  public String toString(boolean useEscSequences) {
    return CVC4JNI.CVC4String_toString__SWIG_0(swigCPtr, this, useEscSequences);
  }

  public String toString() {
    return CVC4JNI.CVC4String_toString__SWIG_1(swigCPtr, this);
  }

  public boolean empty() {
    return CVC4JNI.CVC4String_empty(swigCPtr, this);
  }

  public boolean isEmptyString() {
    return CVC4JNI.CVC4String_isEmptyString(swigCPtr, this);
  }

  public boolean isLeq(CVC4String y) {
    return CVC4JNI.CVC4String_isLeq(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public long size() {
    return CVC4JNI.CVC4String_size(swigCPtr, this);
  }

  public boolean isRepeated() {
    return CVC4JNI.CVC4String_isRepeated(swigCPtr, this);
  }

  public boolean tailcmp(CVC4String y, int[] c) {
    return CVC4JNI.CVC4String_tailcmp(swigCPtr, this, CVC4String.getCPtr(y), y, c);
  }

  public long find(CVC4String y, long start) {
    return CVC4JNI.CVC4String_find__SWIG_0(swigCPtr, this, CVC4String.getCPtr(y), y, start);
  }

  public long find(CVC4String y) {
    return CVC4JNI.CVC4String_find__SWIG_1(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public long rfind(CVC4String y, long start) {
    return CVC4JNI.CVC4String_rfind__SWIG_0(swigCPtr, this, CVC4String.getCPtr(y), y, start);
  }

  public long rfind(CVC4String y) {
    return CVC4JNI.CVC4String_rfind__SWIG_1(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public CVC4String replace(CVC4String s, CVC4String t) {
    return new CVC4String(CVC4JNI.CVC4String_replace(swigCPtr, this, CVC4String.getCPtr(s), s, CVC4String.getCPtr(t), t), true);
  }

  public CVC4String substr(long i) {
    return new CVC4String(CVC4JNI.CVC4String_substr__SWIG_0(swigCPtr, this, i), true);
  }

  public CVC4String substr(long i, long j) {
    return new CVC4String(CVC4JNI.CVC4String_substr__SWIG_1(swigCPtr, this, i, j), true);
  }

  public CVC4String prefix(long i) {
    return new CVC4String(CVC4JNI.CVC4String_prefix(swigCPtr, this, i), true);
  }

  public CVC4String suffix(long i) {
    return new CVC4String(CVC4JNI.CVC4String_suffix(swigCPtr, this, i), true);
  }

  public boolean noOverlapWith(CVC4String y) {
    return CVC4JNI.CVC4String_noOverlapWith(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public long overlap(CVC4String y) {
    return CVC4JNI.CVC4String_overlap(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public long roverlap(CVC4String y) {
    return CVC4JNI.CVC4String_roverlap(swigCPtr, this, CVC4String.getCPtr(y), y);
  }

  public boolean isNumber() {
    return CVC4JNI.CVC4String_isNumber(swigCPtr, this);
  }

  public Rational toNumber() {
    return new Rational(CVC4JNI.CVC4String_toNumber(swigCPtr, this), true);
  }

  public vectorUnsignedInt getVec() {
    return new vectorUnsignedInt(CVC4JNI.CVC4String_getVec(swigCPtr, this), false);
  }

  public long front() {
    return CVC4JNI.CVC4String_front(swigCPtr, this);
  }

  public long back() {
    return CVC4JNI.CVC4String_back(swigCPtr, this);
  }

  public static boolean isDigit(long character) {
    return CVC4JNI.CVC4String_isDigit(character);
  }

  public static long maxSize() {
    return CVC4JNI.CVC4String_maxSize();
  }

}