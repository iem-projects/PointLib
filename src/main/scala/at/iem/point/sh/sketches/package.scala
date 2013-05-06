package at.iem.point.sh

import spire.math.{Rational, compat}

package object sketches {
  implicit val rationalNumeric = compat.numeric[Rational]
}
