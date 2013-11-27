package de.sciss.jacop

import language.implicitConversions

/** Implicit conversions of Int and Bool to IntVar and BoolVar.
  * Used in overloaded operators.
  */
object Implicits {
  /** Converts integer to IntVar.
    *
    * @param i intger to be converted.
    */
  implicit def intToVar(i: Int)(implicit model: Model): IntVar = {
    val v = new IntVar(i, i)
    v
  }

  /** Converts integer to BoolVar.
   *
   * @param b boolean to be converted.
   */
  implicit def booleanToVar(b: Boolean)(implicit model: Model): BooleanVar = {
    val i = if (b) 1 else 0
    val v = new BooleanVar(i, i)
    v
  }

  //  /** Converts Array to List, if needed.
  //    *
  //    * @param a array to be converted.
  //    */
  //  implicit def arrayToList[A](a: Array[A]) = a.toList
}