package at.iem.point.ot

import de.sciss.file._
import de.sciss.midi
import at.iem.point.illism._
import scala.annotation.tailrec
import language.higherKinds
import play.api.libs.json.{JsNumber, JsSuccess, JsError, JsResult, JsValue, Format}
import scala.collection.generic.CanBuildFrom
import de.sciss.play.json.Formats

package object sketches {
  val WARN_MUTA       = true
  val DEBUG_MUTA      = false

  val Vec             = collection.immutable.IndexedSeq
  type Vec[+A]        = collection.immutable.IndexedSeq[A]

  val lilypond        = (userHome / "bin" / "lilypond").path
  val pdfViewer       = "open"

  val pointHome       = userHome / "Desktop" / "IEM" / "POINT"
  val baseDir         = pointHome / "composers" / "orestis_toufektsis"
  val materialDir     = baseDir / "material"
  val outDir          = baseDir / "out"

  val chordsSeqFile   = materialDir / "13-07-28-AKKORDSERIEN-orestis.mid"
  lazy val chordSeq   = midi.Sequence.readFile(chordsSeqFile)
  lazy val chords     = {
    val groups = chordSeq.notes.splitGroups()
    assert(groups.size == 7)
    val _chords = groups.map(n => ChordUtil.findHarmonicFields(n))  // .findChords(n))
    _chords.zipWithIndex.foreach { case (c, i) =>
      val sz = chordVoices(i)
      assert(c.forall(_.size == sz), s"In group ${i + 1}, chord size is not $sz")
    }
    _chords
  }

  lazy val melodies   = chords.map { cs =>
    cs.map(_.notes).transpose
  }

  def chordVoices(idx: Int) = idx + 1 match {
    case 1  => 3
    case 4  => 4
    case 6  => 6
    case 2 | 3 | 5 | 7 => 5
  }

  implicit class OTRichNotes(val seq: Vec[OffsetNote]) extends AnyVal {
    def splitGroups(minimumPause: Double = 1.0): Vec[Vec[OffsetNote]] = {
      @tailrec def loop1(head: Vec[OffsetNote], tail: Vec[OffsetNote],
                         stop: Double): (Vec[OffsetNote], Vec[OffsetNote]) =
        tail match {
          case hd +: tl if hd.offset < stop + minimumPause =>
            val nextStop = math.max(stop, hd.stop)
            loop1(head :+ hd, tl, nextStop)
          case _ =>
            (head, tail)
        }

      @tailrec def loop(res: Vec[Vec[OffsetNote]], rem: Vec[OffsetNote]): Vec[Vec[OffsetNote]] =
        rem match {
          case hd +: tl =>
            val (prev, next) = loop1(Vec.empty, rem, hd.stop)
            loop(res :+ prev, next)
          case _ =>
            res
        }

      loop(Vec.empty, seq)
    }
  }

  implicit class OTRichChords(val seq: Vec[Chord]) extends AnyVal {
    def spread(spacing: Double = 0.0): Vec[Chord] =
      seq match {
        case head +: tail =>
          tail.scanLeft(head) { (pred, succ) =>
            succ.copy(notes = succ.notes.map(_.copy(offset = pred.maxStop + spacing)))
          }
        case _ => seq
      }
  }

  def mkRandom(seed: Long = System.currentTimeMillis()) = new util.Random(seed)

  def rangeRand(min: Int, max: Int)(implicit random: util.Random): Int = {
    require(min <= max)
    random.nextInt(max - min + 1) + min
  }

  private implicit object PitchFormat extends Format[OffsetNote] {
    def reads(json: JsValue): JsResult[OffsetNote] = json match {
      case JsNumber(v) =>
        val pitch = v.toInt.asPitch
        val n     = OffsetNote(offset = 0, pitch = pitch, duration = 1, velocity = 80)
        JsSuccess(n)
      case other => JsError(s"Not a number $other")
    }

    def writes(note: OffsetNote): JsValue = JsNumber(note.pitch.midi)
  }

  /** JSON serialization for chords. N.B.: this only stores and retrieves pitches, no offsets and durations!! */
  implicit object ChordFormat extends Format[Chord] {
    private val notesFmt = Formats.VecFormat[OffsetNote]

    def reads(json: JsValue): JsResult[Chord] = notesFmt.reads(json).map(Chord)

    def writes(c: Chord): JsValue = notesFmt.writes(c.notes)
  }
}
