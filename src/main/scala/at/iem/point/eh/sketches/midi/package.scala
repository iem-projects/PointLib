package at.iem.point.eh.sketches

package object midi {
  final val german  = Language.German
  final val english = Language.English

  private final val pcStrings_en = Array("c","c#","d","d#","e","f","f#","g","g#","a","a#","h")
  private final val pcStrings_de = {
    val res = pcStrings_en.clone()
    res(11) = "b"
    res
  }
  private final val registerSuffixes = Array("sub2", "sub", "con") // , "", "", "'", "''", "'''", "''''", "'''''")

  implicit final class RichKey(val key: Int) extends AnyVal {
    def pitchClassString(lang: Language = Language.English, caps: Boolean = false): String = {
      val pc  = key % 12
      val arr = if (lang == Language.English) pcStrings_en else pcStrings_de
      val s   = arr(pc)
      if (caps) s.toUpperCase else s
   	}

   	def pitchString(lang: Language = Language.English): String = {
      val register  = key / 12
      val caps      = register <= 3
      val pc        = pitchClassString(lang, caps)
      if (register < 3) {
        s"$pc ${registerSuffixes(register)}"
      } else if (register < 5) {
        pc
      } else {
        val ticks = register - 4
        pc + ("'" * ticks)
      }
   	}
  }
}
