//package at.iem.point.ms.sketches
//
//import spire.math.Rational
//import spire.syntax._
//
//final case class Cell(id: Int, elements: IIdxSeq[NoteOrRest], dur: Rational) {
//  override def toString = s"Cell#$id($prettyElements}, dur = $dur)"
//
//  /** Number of elements in the cell */
//  def size = elements.size
//
//  /** Pretty formatted string representation of the cell's elements */
//  def prettyElements: String = elements.map(_.toNumber).mkString("[", ", ", "]")
//
//  /** Pretty formatted string representation of the cell */
//  def pretty: String = {
//    val s = f"#$id%2s: $prettyElements, ${dur.toString}" // "${dur.toString}%3s"
//    s + (" " * math.max(0, 40 - s.length))
//  }
//
//  /** Multiplies the elements by a factor so that their sum will become the nominal total duration. */
//  def normalized: Cell = {
//    val factor = dur / elements.map(_.dur).sum
//    copy(elements = elements.map(_ * factor) /* , dur = 1 */)
//  }
//
//  /** Scales the total duration of the cell by a given factor */
//  def * (factor: Rational): Cell = copy(dur = dur * factor)
//}