package at.iem.point.er

import concurrent.ExecutionContext
import de.sciss.file._

package object sketches {
  implicit val global = ExecutionContext.Implicits.global

  val  Vec      = collection.immutable.IndexedSeq
  type Vec[+A]  = collection.immutable.IndexedSeq[A]

  implicit final class RichDoublePoint(val d: Double) extends AnyVal {
    def explin(inLo: Double, inHi: Double, outLo: Double, outHi: Double): Double =
   		math.log(d / inLo) / math.log(inHi / inLo) * (outHi - outLo) + outLo
  }

  lazy val baseDir = userHome / "Desktop" / "IEM" / "POINT" / "composers" / "eva_reiter"

  // times in seconds as points in the arranged audio file
  // corresponding with the beginning of each system in the score
  lazy val scoreTimes = Vec(
                   1.7, // page 1: begin sys 1,
    13.0,  22.0,  31.0, // page 1: end sys 1, end sys 2, end sys 3
    43.0,  52.5,  62.4, // page 2: end sys 1, end sys 2, end sys 3
    71.0,  82.2,  92.0, // page 3: end sys 1, end sys 2, end sys 3
   102.6, 111.6, 121.0, // page 4: end sys 1, end sys 2, end sys 3
   129.5, 142.4, 151.0, // page 5: end sys 1, end sys 2, end sys 3
   160.0, 170.6, 180.6, // page 6: end sys 1, end sys 2, end sys 3
   191.5, 203.3, 213.0, // page 7: end sys 1, end sys 2, end sys 3
   226.3, 241.5, 255.0  // page 8: end sys 1, end sys 2, end sys 3
  )
}
