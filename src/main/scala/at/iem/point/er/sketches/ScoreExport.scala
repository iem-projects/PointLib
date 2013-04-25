package at.iem.point.er.sketches

import java.io.{OutputStreamWriter, FileOutputStream, File}
import collection.immutable.{IndexedSeq => IIdxSeq}
import spire.math.Rational
import scala.annotation.tailrec

object ScoreExport {
  var lilypond  = sys.props("user.home") + "/bin/lilypond"
  var pdfViewer = "open"
  var debug     = true    // leaves lilypond files on the desktop!

  //  Dictionary[
  //  "64" -> 1,
  //  "64." -> 1.5,
  //  "32" -> 2,
  //  "32." -> 3,
  //  "16" -> 4,
  //  "16." -> 6,
  //  "8" -> 8,
  //  "8." -> 12,
  //  "4" -> 16,
  //  "4." -> 24,
  //  "2" -> 32,
  //  "2." -> 48,
  //  "1" -> 64,
  //  "1." -> 96
  // ]

  //  private val r1_64 = Rational(1, 64)
  //  private val r1_96 = Rational(1, 96)
  //  private val r3_2  = Rational(3, 2)
  private val r2_3  = Rational(2, 3)
  private val r3_4  = Rational(3, 4)
  private val r1_4  = Rational(1, 4)

  //  implicit final class RichInt(val numer: Int) extends AnyVal {
  //    def /- (denom: Int) = Rational(numer, denom)
  //  }

  private val divisions = List(1, 2, 4, 8, 16, 32, 64).map(Rational(1, _))

  def apply(file: File, onsets: IIdxSeq[Long], sampleRate: Double, autoBeamOff: Boolean = false, subtitle: String = "") {
    require(onsets.size >= 2, s"Must have at least two onsets (number is ${onsets.size})")

    val dir       = if (debug) new File(sys.props("user.home"), "Desktop") else null
    val ly        = File.createTempFile("point", ".ly", dir)

    def onsetsSec = onsets.sliding(2, 1).map { case Seq(prev, succ) =>
      val frames: Long = succ - prev
      val secs    = frames / sampleRate
      secs
    } .toIndexedSeq

    val minDurSec = onsetsSec.min

    // expects nominator to be either of 1, 3, 7
    def dotString(r: Rational): String = {
      val denom = r.denominator.intValue()
      r.numerator.intValue() match {
        case 1 => s"$denom"
        case 3 => s"${denom/2}."
        case 7 => s"${denom/4}.."
      }
    }

    // tempo given in beats per minute, where beat = quarter
    @tailrec def autoTempo(tempo: Double = 90): Double = {
      val wholeDur  = 1.0/(tempo/(4 * 60))
      val minValue  = minDurSec / wholeDur
      if (minValue >= 1.0/96) tempo else {
        autoTempo(tempo * 4/3)
      }
    }

    def schoko(): (Rational, Int, IIdxSeq[String]) = {

      val tempoFrac = autoTempo()

      @tailrec def tempoSig(note: Rational = r1_4): (Rational, Int) = {
        val factor  = 4 * note
        val tempo   = tempoFrac * factor.doubleValue()
        if (tempo <= 160) {
          val tempo1  = (tempo + 0.5).toInt + 9
          val tempo2  = tempo1 - tempo1 % 10  // round up to multiples of 10
          (note, tempo2)
        } else {
          val factor = note.numerator.intValue() match {
            case 1 => r3_4  // e.g. 4 -> 8.
            case 3 => r2_3
          }
          tempoSig(note * factor)
        }
      }

      val (_tempoBase, _tempoNom) = tempoSig()

      val wholeDur  = 60 / (_tempoBase * _tempoNom).doubleValue()
      val values    = onsetsSec.map(_ * wholeDur)

      if (debug) {
        println(f"tempoFrac $tempoFrac%1.2f yields base ${_tempoBase} with nominal tempo ${_tempoNom}; wholeDur = $wholeDur%1.2f")
        println(s"First five onsets frames ${onsets.take(5)} -> seconds ${onsetsSec.take(5)} -> values ${values.take(5)}")
      }

      val _notes: IIdxSeq[String] = values.map { v =>
        val frac  = Rational(v)
        val lim   = frac.limitDenominatorTo(96)
        // val rest  = lim - note

        @tailrec def decompose(rem: Rational, units: List[Rational], sq: Vector[Rational]): Vector[Rational] =
          units match {
            case unit :: tail =>
              if (rem >= unit) {
                decompose(rem - unit, units, sq :+ unit)
              } else {
                decompose(rem, tail, sq)
              }
            case _ => sq
          }

        def dot(sq: Vector[Rational]): Vector[Rational] =
          sq match {
            case init :+ a :+ b :+ c if a.numerator == 1 && a == b * 2 && b == c * 2 => // Doppelpunktierung
              init :+ (a + b + c)
            case init :+ a :+ b      if a.numerator == 1 && a == b * 2               => // Einfachpunktierung
              init :+ (a + b)
            case _ => sq
          }

        val dec     = decompose(lim, divisions, Vector.empty)
        val dotted  = dot(dec)

        val durs    = dotted map { r =>
          val dur = dotString(r)
          s"c'$dur"
        }

        durs.mkString("", "~", " s32")  // tie. the added silence of 1/32 yields better spacing for the short ties
      }

      (_tempoBase, _tempoNom, _notes)
    }

    val (tempoBase, tempoNom, notes) = schoko()

    val n   = file.getName
    val ni0 = n.lastIndexOf('.')
    val ni  = if (ni0 < 0) n.length else ni0
    require(ni0 < 0 || n.substring(ni) == ".pdf") // sucky lilypond adds .pdf by itself
    val name    = n.substring(0, ni)
    val outPath = new File(file.getParentFile, name).getAbsolutePath

    val score =
      raw"""\header {
        |  title = \markup { \fontsize #-1 \sans "$name" }
        |  tagline = ""
        |  subtitle = "${if (subtitle.isEmpty) " " else subtitle}"
        |}
        |\version "2.16.2"
        |
        |\layout {
        |  ragged-right = ##t
        |
        |  \context { \Score
        |    % % tuplet handling
        |    % tupletFullLength = ##t
        |    % \override TupletBracket #'bracket-visibility = ##t
        |    % \override TupletBracket #'padding = #2
        |    % % allow tuplet bracket to always be visible, even for short tuplets.
        |    % \override TupletBracket #'springs-and-rods = #ly:spanner::set-spacing-rods
        |    % \override TupletBracket #'minimum-length = #3
        |	   % \override TupletNumber #'text = #tuplet-number::calc-fraction-text
        |
        |	   % \remove Bar_number_engraver
        |    % \override TimeSignature #'X-extent = #'(0 . 3)
        |    % \override InstrumentName #'X-extent = #'(0 . 4)
        |
        |    % proportionalNotationDuration = #(ly:make-moment 1 56)
        |    % \override SpacingSpanner #'strict-note-spacing = ##t
        |    % \override SpacingSpanner #'strict-grace-spacing = ##t
        |    % \override SpacingSpanner #'uniform-stretching = ##t
        |
        |    % non-proportional settings:
        |    \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 32)
        |  }
        |  \context { \Voice
        |    \remove "Forbid_line_break_engraver"
        |  }
        |}
        |
        |\paper {
        |  after-title-spacing = #'((space . 0) (padding . 1.5) (stretchability . 3) (minimum-distance . 0))
        |}
        |
        |#(set-default-paper-size "a4")
        |#(set-global-staff-size 16)
        |
        |\score {
        |  \new RhythmicStaff {
        |    \tempo ${dotString(tempoBase)} = $tempoNom
        |    ${if (autoBeamOff) "\\autoBeamOff" else ""}
        |    ${notes.mkString(" ")}
        |  }
        |}
        |
      """.stripMargin

    val os = new OutputStreamWriter(new FileOutputStream(ly), "UTF-8")
    os.write(score)
    os.close()

    import sys.process._

    val cmd = Seq(lilypond, "-o", outPath, ly.getAbsolutePath)
    println(cmd.mkString(" "))
    val res = cmd.!
    if (!debug) ly.delete()
    if (res != 0) sys.error(s"Lilypond exited with code $res")

    Seq(pdfViewer, outPath + ".pdf").!
  }
}