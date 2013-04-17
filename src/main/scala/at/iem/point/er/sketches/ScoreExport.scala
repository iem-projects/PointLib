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

  private val r1_64 = Rational(1, 64)
  private val r1_96 = Rational(1, 96)
  private val r3_2  = Rational(3, 2)

  //  implicit final class RichInt(val numer: Int) extends AnyVal {
  //    def /- (denom: Int) = Rational(numer, denom)
  //  }

  private val divisions = List(1, 2, 4, 8, 16, 32, 64).map(Rational(1, _))

  def apply(file: File, onsets: IIdxSeq[Long], sampleRate: Double, tempo: Double = 120) {
    require(onsets.size >= 2, s"Must have at least two onsets (number is ${onsets.size})")
    val wholeDur  = 1.0/(tempo/(4 * 60)) // tempo given in beats per minute, where beat = quarter
    val dir       = if (debug) new File(sys.props("user.home"), "Desktop") else null
    val ly        = File.createTempFile("point", ".ly", dir)
    val onsets0   = /* 0L +: */ onsets
    val values    = onsets0.sliding(2, 1).map { case Seq(prev, succ) =>
      val frames: Long = succ - prev
      val secs    = frames / sampleRate
      secs / wholeDur
    } .toIndexedSeq
    val minValue  = values /*.tail */ .min

    require(minValue >= 1.0/96, f"Smallest note duration ($minValue%1.2f)less than 1/96 at given tempo ($tempo%1f)")

    val notes   = values.map { v =>
      val frac  = Rational(v)
      val small = frac < r1_96
      //      val note  = if (small) r1_96 else r1_64
      val lim   = frac.limitDenominatorTo((if (small) r1_96 else r1_64).denominator)
      //      val rest  = lim - note

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

      def dot(sq: Vector[Rational]): Vector[Rational] = {
        sq match {
          case init :+ a :+ b :+ c if b == c * r3_2 && a == b * r3_2 => // Doppelpunktierung
            init :+ (a + b + c)
          case init :+ a :+ b if a == b * r3_2  => // Einfachpunktierung
            init :+ (a + b)
          case _ => sq
        }
      }

      val dec     = decompose(lim, divisions, Vector.empty)
      val dotted  = dot(dec)

      val durs    = dotted map { r =>
        val denom = r.denominator.intValue()
        val dur   = r.numerator.intValue() match {
          case 1 => s"$denom"
          case 3 => s"${denom/2}."
          case 7 => s"${denom/4}.."
        }
        s"c'$dur"
      }

      durs.mkString("~")  // tie
    }

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
        |  subtitle = " " % padding the cheesy way
        |}
        |\version "2.16.2"
        |
        |\layout {
        |  ragged-right = ##t
        |
        |  \context { \Score
        |    % tuplet handling
        |    tupletFullLength = ##t
        |    \override TupletBracket #'bracket-visibility = ##t
        |    \override TupletBracket #'padding = #2
        |    % allow tuplet bracket to always be visible, even for short tuplets.
        |    \override TupletBracket #'springs-and-rods = #ly:spanner::set-spacing-rods
        |    \override TupletBracket #'minimum-length = #3
        |	 \override TupletNumber #'text = #tuplet-number::calc-fraction-text
        |
        |	 \remove Bar_number_engraver
        |    \override TimeSignature #'X-extent = #'(0 . 3)
        |    \override InstrumentName #'X-extent = #'(0 . 4)
        |
        |    proportionalNotationDuration = #(ly:make-moment 1 56)
        |    \override SpacingSpanner #'strict-note-spacing = ##t
        |    \override SpacingSpanner #'strict-grace-spacing = ##t
        |    \override SpacingSpanner #'uniform-stretching = ##t
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
        |    \tempo 4 = 120
        |    % \set Staff.instrumentName = \markup { \char ##x00D7 1/4 }
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