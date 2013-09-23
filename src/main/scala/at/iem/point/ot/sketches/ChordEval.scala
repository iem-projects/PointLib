package at.iem.point.ot.sketches

sealed trait ChordEval
case object ChordNeutral extends ChordEval
case object ChordBad     extends ChordEval
case object ChordGood    extends ChordEval