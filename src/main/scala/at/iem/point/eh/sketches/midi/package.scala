package at.iem.point.eh.sketches

import java.text.DecimalFormat
import java.math.RoundingMode

package object midi {
  final val german  = Language.German
  final val english = Language.English

  // cf. http://www.sengpielaudio.com/Rechner-notennamen.htm
  private final val pcStrings_en = Array("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  private final val pcStrings_de = Array("c","cis","d","dis","e","f","fis","g","gis","a","ais","h")

  private lazy val dfRound3 = {
    val res = new DecimalFormat("#.###")
    res.setRoundingMode(RoundingMode.HALF_UP)
    res
  }

  implicit final class RichSeconds(val sec: Double) extends AnyVal {
    def roundSecondsToMillis: String = dfRound3.format(sec)
  }

  implicit final class RichIterable[A](val t: Iterable[A]) extends AnyVal {
    def isSortedBy[B](fun: A => B)(implicit ord: Ordering[B]): Boolean = {
      t.sliding(2, 1).forall { case Seq(a, b) => ord.lteq(fun(a), fun(b)) }
    }
  }

  implicit final class RichKey(val key: Int) extends AnyVal {
    def pitchClassString(lang: Language = Language.English, caps: Boolean = false): String = {
      val pc = key % 12
      lang match {
        case Language.English => pcStrings_en(pc)
        case Language.German =>
          val s = pcStrings_de(pc)
          if (caps) s.capitalize else s
      }
   	}

   	def pitchString(lang: Language = Language.English): String = {
      val register  = key / 12
      lang match {
        case Language.English =>
          pitchClassString(lang) + register
        case Language.German =>
          val caps  = register <= 3
          val pc    = pitchClassString(lang, caps)
          if (register <= 2) {
            val ticks = 3 - register
            ("," * ticks) + pc
          } else if (register >= 5) {
            val ticks = register - 4
            pc + ("'" * ticks)
          } else {
            pc
          }
      }
   	}
  }
}
