package at.iem.point.ot.sketches

import at.iem.point.illism.{Chord, Pitch}

object Horizontal {
  //  4. Registerangabe pro Stimme
  //  5. Stimmabstände ("Präferenzen"; z.B. Prozentangaben)

  //  - erlaubte Intervalle; evtl. separat pro Stimme
  //  - Richtungen (liegen bleiben, aufwaerts, abwaerts)
  //    ; z.B. liegen bleiben sollen mind. 1 max. 3 Stimmen
  //  (also jeweils bounds fuer Zahl der Stimmen je Richtung)

  def fromChords(chords: Vec[Chord]): Vec[Vec[Pitch]] = {
    val sz = chords.size
    require(sz > 0, "Must at least provide one chord")
    val pitches = chords.map(_.pitches)
    pitches.transpose

    //    val voices = chords.head.size
    //    Vec.tabulate(voices) { v =>
    //      Vec.tabulate(sz) { h =>
    //        val p = pitches(h)
    //        if (v == 0) require(p.size == voices, s"Chord No. ${h+1} has ${p.size} voices, expected $voices")
    //        pitches(h)(v)
    //      }
    //    }
  }
}