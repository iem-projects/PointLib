package at.iem.point.er.sketches

import java.io.{OutputStreamWriter, FileOutputStream, File}
import collection.immutable.{IndexedSeq => IIdxSeq}

object ScoreExport {
  var lilypond  = sys.props("user.home") + "/bin/lilypond"
  var pdfViewer = "open"

  def apply(file: File, onsets: IIdxSeq[Long], sampleRate: Double, tempo: Double = 120) {
    val ly      = File.createTempFile("point", ".ly")

    val values  = onsets.map { frames =>
      val secs = frames / sampleRate
      "c'8" // test
    }

    val n   = file.getName
    val ni0 = n.lastIndexOf('.')
    val ni  = if (ni0 < 0) n.length else ni0
    require(ni0 < 0 || n.substring(ni) == ".pdf") // sucky lilypond adds .pdf by itself
    val name    = n.substring(0, ni)
    val outPath = new File(file.getParentFile, name).getAbsolutePath

    val score =
      raw"""\header {
        |  title = \markup { \fontsize #-1 \sans "Onsets for $name" }
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
        |    \tempo 8. = 120
        |    % \set Staff.instrumentName = \markup { \char ##x00D7 1/4 }
        |    ${values.mkString(" ")}
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
    if (res != 0) sys.error(s"Lilypond exited with code $res")

    Seq(pdfViewer, outPath + ".pdf").!
  }
}