package at.iem.point.sh

import de.sciss.desktop.Desktop
import spire.math.Rational
import at.iem.point.illism.rhythm.Cell
import de.sciss.file._
import play.api.libs.json.{JsSuccess, JsString, JsError, JsNumber, JsObject, JsResult, JsValue, Format}
import scala.util.{Failure, Try, Success}
import spire.syntax.literals._
import spire.compat

package object sketches {
  type Vec[+A] = collection.immutable.IndexedSeq[A]
  val  Vec     = collection.immutable.IndexedSeq

  implicit val rationalNumeric    = compat.numeric   [Rational]
  implicit val rationalFractional = compat.fractional[Rational]

  val lilypond  = if (Desktop.isLinux) "lilypond" else (userHome / "bin" / "lilypond").path
  val pdfViewer = if (Desktop.isLinux) "xdg-open" else "open"

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

  // assert(factors.size == 106)

  implicit class RichRational(val r: Rational) extends AnyVal {
    def multiples(lo: Rational, hi: Rational): Vec[Rational] = {
      require(lo >= 0 && hi >= lo)
      val loF = (lo / r).ceil .toInt
      val hiF = (hi / r).floor.toInt + 1
      (loF until hiF).map(r * _)
    }
  }

  implicit object RationalFormat extends Format[Rational] {
    def reads(json: JsValue): JsResult[Rational] = json match {
      case JsString(s) => Try(Rational(s)) match {
        case Success(r) => JsSuccess(r)
        case Failure(e) => JsError(s"Not a rational: '$s' - ${e.getMessage}")
      }
      case other => JsError(s"Not a JSON string $other")
    }

    def writes(r: Rational): JsValue = JsString(r.toString)
  }

  implicit object cellFormat extends Format[Cell] {
    def reads(json: JsValue): JsResult[Cell] = json match {
      case JsObject(fs) =>
        val map = fs.toMap
        map.get("id") match {
          case Some(JsNumber(num)) =>
            val id = num.toInt
            map.get("dur").fold[JsResult[Cell]](JsError(s"Missing dur field in $json")) { jsf =>
              RationalFormat.reads(jsf).map { dur =>
                baseCells(id).copy(dur = dur)
              }
            }

          case Some(other)  => JsError(s"Not a JSON number $other")
          case None         => JsError(s"Missing id field in $json")
        }

      case other => JsError(s"Not a JSON object $other")
    }

    def writes(cell: Cell): JsValue = JsObject(Seq(
      "id"  -> JsNumber(cell.id),
      "dur" -> RationalFormat.writes(cell.dur)
    ))
  }
}