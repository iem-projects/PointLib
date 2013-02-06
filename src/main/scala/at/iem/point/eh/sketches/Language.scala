package at.iem.point.eh.sketches

object Language {
  var default: Language = Language.English

  case object English extends Language
  case object German  extends Language
}
sealed trait Language