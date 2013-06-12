package at.iem.point.sh.sketches

import de.sciss.file._
import java.io.{FileOutputStream, OutputStreamWriter}
import Fitness.GenomeVal

object ExportLilypond {
  /** Exports a number of chromosomes as Lilypond score and (optionally) MIDI.
    *
    * @param genome   the sequence of chromosomes
    * @param out      the file (extension will be stripped!)
    * @param title    title for the score
    * @param subTitle sub title for the score
    * @param midi     whether to output to MIDI file as well or not
    */
  def apply(genome: GenomeVal, out: File, title: String = "Title", subTitle: String = "Sub Title", midi: Boolean = true) {
    // ---- lilypond test output ----
    val header = raw"""\header {
      |  title = \markup { \fontsize #-1 \sans { $title }}
      |  tagline = ""
      |  subtitle = \markup { \sans { $subTitle }}
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
      |    % nothing of this shit works
      |    % \override NonMusicalPaperColumn #'line-break-system-details #'((Y-offset . 0) (alignment-distances . (30 10)))
      |    % \override VerticalAxisGroup #'staff-staff-spacing = #'((basic-distance . 10) (minimum-distance . 30) (padding . 30))
      |
      |    \override RehearsalMark #'padding = #7
      |
      |    % non-proportional settings:
      |    \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 32)
      |  }
      |  \context { \Voice
      |    \remove "Forbid_line_break_engraver"
      |  }
      |}
      |
      |#(set-default-paper-size "a4")
      |#(set-global-staff-size 16)
      |
      |annotation = #(define-music-function (parser location text) (string?)
      |#{
      |  \mark \markup { \fontsize #-2 \italic #text }
      |#})
      |
      |\paper {
      |  markup-system-spacing #'padding = #5
      |}
    """.stripMargin

    val scores = genome.zipWithIndex.map { case ((sq, fit), idx) =>
      val fitS  = f"$fit%1.5f"
      val inner = sq.map(cell => cell.toLilypondString(timeSig = true, annotation = s"#${cell.id+1}")).mkString("\n")
      raw"""
      |\score {
      |  \new RhythmicStaff {
      |    $inner
      |  }
      |  \header { piece = "No. ${idx + 1} - fitness $fitS" }
      |  ${if (midi) "\\midi { }" else ""}
      |  \layout { }
      |}
    """.stripMargin }

    val lyt = scores.mkString(header, "", "")
    // val lyf = IO.tempFile("point", ".ly", deleteOnExit = false)
    val lyf = out.replaceExt("ly")
    val os  = new OutputStreamWriter(new FileOutputStream(lyf), "UTF-8")
    os.write(lyt)
    os.close()

    import sys.process._

    val outPath = out.parent // userHome / "Desktop"
    val cmd = Seq(lilypond, "-o", outPath.path, lyf.absolutePath)
    println(cmd.mkString(" "))
    val cmdRes = cmd.!
    if (cmdRes != 0 && cmdRes != 1) sys.error(s"Lilypond exited with code $cmdRes")

    Seq(pdfViewer, (outPath / lyf.base).replaceExt("pdf").path).!
  }
}