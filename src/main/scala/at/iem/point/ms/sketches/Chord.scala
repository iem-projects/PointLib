package at.iem.point.ms.sketches

import annotation.tailrec

/**
 * A chord made of a sequence of notes. Notes must be in ascending order with respect to their
 * pitches.
 */
final case class Chord(notes: IIdxSeq[OffsetNote]) {
  require(notes.isSortedBy(_.pitch))

  def minOffset: Double = notes.minBy(_.offset).offset
  def maxStop:   Double = notes.maxBy(_.stop).stop

  /**
   * The number of notes in the chord.
   */
  def size: Int = notes.size

  /**
   * Calculates the arithmetic mean of all offset times.
   */
  def avgOffset: Double = notes.map(_.offset).sum / notes.size

  /**
   * Calculates the arithmetic mean of all stop times.
   */
  def avgStop: Double = notes.map(_.stop).sum / notes.size

  /**
   * Calculates the __geometric__ mean of all durations.
   */
  def avgDuration: Double = Math.pow(notes.map(_.duration).product, 1.0 / notes.size)

  /**
   * Collects the pitches of the chord.
   *
   * @return  the pitches in ascending order
   */
  def pitches: IIdxSeq[Pitch] = notes.map(_.pitch)

  /**
   * Returns the framing interval which is the interval between lowest and highest pitch in the chord.
   */
  def frameInterval: UndirectedInterval = (notes.last.pitch interval notes.head.pitch).undirected

  /**
   * Returns a sequence of subsequent intervals
   */
  def layeredIntervals: IIdxSeq[UndirectedInterval] = pitches.intervals.map(_.undirected)

  /**
   * Returns a sequence of all intervals between all pairs of pitches
   */
  def allIntervals: IIdxSeq[Interval] = {
    val b = IIdxSeq.newBuilder[Interval]
    @tailrec def loop(sq: List[Pitch]) {
      sq match {
        case head :: tail =>
          tail.foreach(t => b += t interval head)
          loop(tail)
        case _ =>
      }
    }
    loop(pitches.toList)
    b.result().sorted
  }
}