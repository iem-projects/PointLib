package at.iem.point.er

import concurrent.ExecutionContext
import de.sciss.file._

package object sketches {
  implicit val global = ExecutionContext.Implicits.global

  val  Vec      = collection.immutable.IndexedSeq
  type Vec[+A]  = collection.immutable.IndexedSeq[A]

  implicit final class RichIntPoint(val i: Int) extends AnyVal {
    def isPowerOfTwo: Boolean = (i & (i-1)) == 0
    def nextPowerOfTwo: Int = {
   		var j = 1
   		while( j < i ) j <<= 1
   		j
   	}
  }

  implicit final class RichDoublePoint(val d: Double) extends AnyVal {
    def explin(inLo: Double, inHi: Double, outLo: Double, outHi: Double): Double =
   		math.log(d / inLo) / math.log(inHi / inLo) * (outHi - outLo) + outLo
  }

  lazy val baseDir = userHome / "Desktop" / "IEM" / "POINT" / "composers" / "eva_reiter"
}
