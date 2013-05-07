package at.iem.point.sh.sketches

import collection.immutable.{IndexedSeq => IIdxSeq}
import spire.math.Rational
import spire.syntax._

object Cell {
  import scala.{Vector => v}
  val cell = v(
    Cell( 0, v(1, 1, 1), r"1/4"),
    Cell( 1, v(1, 3), r"1/4"),
    Cell( 2, v(3, 1), r"1/4"),
    Cell( 3, v(1, 1, 1, 1, 1), r"1/4"),
    Cell( 4, v(4, 1), r"1/4"),
    Cell( 5, v(1, 1, 4), r"1/4"),
    Cell( 6, v(-1, 1, 1, -1, 1, 1), r"1/4"),
    Cell( 7, v(1, 1, 1, 1, -1, 1, 2), r"1/4"),
    Cell( 8, v(-4, 1, 1, 1), r"1/4"),
    Cell( 9, v(-1, 2, 1), r"1/2"),
    Cell(10, v(-1, 3), r"1/2"),
    Cell(11, v(3, 2, 1), r"1/2"),
    Cell(12, v(-2, 2, -1, 3), r"1/2"),
    Cell(13, v(-3, 2, 1, -2), r"1/2"),
    Cell(14, v(3, 1, -1, 1, -2), r"1/2"),
    Cell(15, v(6, 1, 1, -2), r"1/2"),
    Cell(16, v(2, 7, 3), r"1/2"),
    Cell(17, v(2, 9, 1), r"1/2"),
    Cell(18, v(1, 1, 7, 1, 1, 5), r"1/2"),
    Cell(19, v(1, 1, 3, 1, 1, 5), r"3/4"),
    Cell(20, v(7, 6, 5, 4, 3, 2, 1), r"7/8"),
    Cell(21, v(1, 2, 2, 5, -2), 1),
    Cell(22, v(1, 2, 3, 4, 2, 3, 5, 1, 7, -4), 1),
    Cell(23, v(9, 15), r"6/4"),
    Cell(24, v(12, 5, 11), r"7/4")
  )

  def baseCells = cell

  // make sure we have no tipos
  assert(baseCells.zipWithIndex.forall { case (Cell(id, _, _), idx) => id == idx })

  // cells with total durations integrated into the elements
  val norm = baseCells.map(_.normalized)

  val baseFactors = Vector(5, 6, 7, 8, 9).map(Rational(1, _))
  val minStretch  = r"1/4"
  val maxStretch  = r"4"

  // there are 106 different factors when expanding the base factors
  // through multiplication within the min/max stretch
  val factors     = baseFactors.flatMap(_.multiples(minStretch, maxStretch)).sorted.distinct

  assert(factors.size == 106)
}
final case class Cell(id: Int, elements: IIdxSeq[NoteOrRest], dur: Rational) {
  override def toString = s"Cell#$id($prettyElements}, dur = $dur)"

  /** Number of elements in the cell */
  def size = elements.size

  /** Pretty formatted string representation of the cell's elements */
  def prettyElements: String = elements.map(_.toNumber).mkString("[", ", ", "]")

  /** Pretty formatted string representation of the cell */
  def pretty: String = {
    val s = f"#$id%2s: $prettyElements, ${dur.toString}" // "${dur.toString}%3s"
    s + (" " * math.max(0, 40 - s.length))
  }

  /** Multiplies the elements by a factor so that their sum will become the nominal total duration. */
  def normalized: Cell = {
    val factor = dur / elements.map(_.dur).sum
    copy(elements = elements.map(_ * factor) /* , dur = 1 */)
  }

  /** Scales the total duration of the cell by a given factor */
  def * (factor: Rational): Cell = copy(dur = dur * factor)
}