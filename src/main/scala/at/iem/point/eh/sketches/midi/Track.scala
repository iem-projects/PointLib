package at.iem.point.eh.sketches.midi

import collection.immutable.{IndexedSeq => IIdxSeq}

trait Track {
  def ticks: Long
  def events: IIdxSeq[Event]
  def notes: IIdxSeq[OffsetNote]

  def schoko =
    """
      |hallo
    """.stripMargin
}