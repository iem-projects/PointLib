package at.iem.point.eh

import java.awt.{Font, Color, EventQueue}
import de.sciss.midi
import at.iem.point.illism.{PitchClass, Pitch, Chord}
import de.sciss.file._
import language.higherKinds
import org.jfree.chart.plot.{CategoryPlot, Plot, XYPlot}
import scalax.chart.Chart
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer}
import org.jfree.chart.title.TextTitle

package object sketches {
  val  Vec    = collection.immutable.IndexedSeq
  type Vec[A] = collection.immutable.IndexedSeq[A]

  var basePath  = file(sys.props("user.home")) /* / "Desktop" */ / "IEM" / "POINT" / "composers" / "elisabeth_harnik"
  def inPath    = basePath / "in"
  def outPath   = basePath / "rec"
  def outPath2  = basePath / "rec_neu"
  def oldPath   = basePath / "elisabeth-first-tests"

  lazy val snippetFiles: Map[Int, File] = {
    val b   = Map.newBuilder[Int, File]
    val Pat = "snippet (\\d+).mid".r
    def loop(d: File): Unit =
      d.children.foreach { f =>
        if (f.isFile) f.name match {
          case Pat(num) => b += num.toInt -> f
          case _ =>
        } else loop(f)
      }
    loop(inPath)
    b.result()
  }

  def loadSnippet(idx: Int): midi.Sequence = midi.Sequence.read(snippetFiles(idx).getPath)

  private val disklavierNames = Vector(
    "Kreisend", "Cluster", "StummeTasten", "4_TeilweisePedal", "MitGanzOberenHoehen",
    "SchleifenderDaumen"
  )

  /** Loads a MIDI snippet from the disklavier session.
    *
    * @param idx  file index from 0 to (including) 4
    */
  def loadDisklavier(idx: Int): midi.Sequence = midi.Sequence.read((outPath2 / s"${disklavierNames(idx)}.mid").getPath)

  def loadFirstTests(name: String): midi.Sequence = {
    val p = (oldPath / name).getAbsolutePath
    println(s"Reading $p")
    midi.Sequence.read(p)
  }

  // maps voices to snippet indices
  lazy val staticChords = Map(
    2 -> List(11, 12, 13),
    3 -> List(14, 15, 16),
    4 -> List(18, 19, 20),
    5 -> List(46),
    6 -> List(21, 22, 23)
  )

  // snippet indices of free improvisation sets
  lazy val improvSnippets = 9 :: 48 :: Nil

  def defer(thunk: => Unit): Unit =
    if (EventQueue.isDispatchThread) thunk else EventQueue.invokeLater(new Runnable { def run() { thunk }})

  implicit final class RichChord(val chord: Chord) extends AnyVal {
     def avgVelocity: Float = {
       val v = chord.notes.map(_.velocity).sum
       v.toFloat / chord.size
     }
  }

  object KeyColor {
    case object Black extends KeyColor
    case object White extends KeyColor
  }
  sealed trait KeyColor

  def keyPositionToPitch(pos: Int): Pitch = {
    val pos1  = if (pos < 25 /* 0 */) {
      println("Warning: moving too low  pitch back into MIDI register")
      val add = (25 - pos + 13) / 14 * 14
      pos + add
    } else if (pos > 126 /* 148 */) {
      println("Warning: moving too high pitch back into MIDI register")
      val sub = (pos - 126 /* 148 */ + 13) / 14 * 14
      pos - sub
    } else {
      pos
    }
    val oct   = pos1 / 14
    val pos2  = pos1 % 14
    require(pos2 != 5 && pos2 != 13, "Ended up at illegal key position")
    val step  = if (pos2 <= 4) pos2 else pos2 - 1

    new Pitch(oct * 12 + step)
  }

  implicit final class RichPitchClass(/* val */ p: PitchClass) /* extends AnyVal */ {
    def keyColor: KeyColor = p.step match {
      case 1 | 3 | 6 | 8 | 10 => KeyColor.Black
      case _                  => KeyColor.White
    }

    def keyPosition: Int = if (p.step <= 4) p.step else p.step + 1
  }

  implicit final class RichPitch(/* val */ pitch: Pitch) /* extends AnyVal */ {
    def keyColor: KeyColor = pitch.`class`.keyColor

    def keyPosition: Int = {
      val oct = pitch.midi / 12
      oct * 14 + pitch.`class`.keyPosition
    }

    /** A white key to another neighbouring white key is distance two,
      * transition between neighbouring white and black keys is distance one.
      */
    def distanceTo(that: Pitch): Int = that.keyPosition - this.keyPosition

    def moveBy(distance: Int): Pitch = {
      val pos0  = this.keyPosition + distance
      keyPositionToPitch(pos0)
    }
  }

  private lazy val defaultFontFace  = "Liberation Sans" // "Helvetica"  // "Arial"
  private lazy val titleFontFace    = defaultFontFace // "Liberation Sans Narrow"

  implicit class RichChart[P <: Plot](chart: Chart[P]) {
    /** Adjust the chart with a black-on-white color scheme and
      * fonts that come out properly in PDF export.
      */
    def printableLook(): Unit = {
      val plot      = chart.plot
      val titleText = chart.title
      // chart.peer.setTitle(new TextTitle(titleText, new Font(titleFontFace, Font.BOLD, 22)))

      val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
        case p: XYPlot       =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.darkGray )
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
        case p: CategoryPlot =>
          p.setBackgroundPaint           (Color.white    )
          p.setDomainGridlinePaint       (Color.lightGray)
          p.setRangeGridlinePaint        (Color.lightGray)
          p.getRenderer.setSeriesPaint(0, Color.darkGray )
          // undo the crappy "3D" look
          p.getRenderer match {
            case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
            case _ =>
          }
          (p.getDomainAxis, p.getRangeAxis)
      }

      //      val xAxis         = plot.getDomainAxis
      //      val yAxis         = plot.getRangeAxis
      val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
      val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
      xAxis.setLabelFont(fnt1)
      xAxis.setTickLabelFont(fnt2)
      yAxis.setLabelFont(fnt1)
      yAxis.setTickLabelFont(fnt2)
      // chart.peer.getTitle.setFont(new Font(titleFontFace, Font.BOLD, 22))
    }
  }
}