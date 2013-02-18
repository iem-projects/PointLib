package at.iem.point.er

import java.io.File
import concurrent.ExecutionContext

package object sketches {
  def file(path: String) = new File(path)

  implicit val global = ExecutionContext.Implicits.global

  implicit final class RichIntPoint(val i: Int) extends AnyVal {
    def isPowerOfTwo = (i & (i-1)) == 0
  }

  implicit final class RichDoublePoint(val d: Double) extends AnyVal {
    def explin(inLo: Double, inHi: Double, outLo: Double, outHi: Double): Double =
   		math.log(d / inLo) / math.log(inHi / inLo) * (outHi - outLo) + outLo
  }

  def createTempFile(prefix: String, suffix: String): File = {
    val f = File.createTempFile(prefix, suffix)
    f.deleteOnExit()
    f
  }
}
