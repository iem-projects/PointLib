package at.iem.point.sh

import spire.math.{Rational, compat}
import scala.collection.immutable.{IndexedSeq => IIdxSeq}

package object sketches {
  implicit val rationalNumeric = compat.numeric[Rational]

  implicit class RichRational(val r: Rational) extends AnyVal {
    def multiples(lo: Rational, hi: Rational): IIdxSeq[Rational] = {
      require(lo >= 0 && hi >= lo)
      val loF = (lo / r).ceil .toInt
      val hiF = (hi / r).floor.toInt + 1
      (loF until hiF).map(r * _)
    }
  }
}