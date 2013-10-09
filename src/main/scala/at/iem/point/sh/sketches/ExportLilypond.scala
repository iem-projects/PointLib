package at.iem.point.sh.sketches

import de.sciss.file._
import java.io.{FileOutputStream, OutputStreamWriter}
import Fitness.GenomeVal
import de.sciss.desktop.{FileDialog, OptionPane, Window}
import scala.annotation.tailrec
import de.sciss.guiflitz.{Springs, SpringPanel}
import at.iem.point.illism.rhythm.LilyTimeSignature
import de.sciss.muta.HeaderInfo

object ExportLilypond {
  def dialog(info: HeaderInfo, genome: GenomeVal, parent: Option[Window] = None) {
    import swing._

    //    val lbTitle = new Label("Title:"          , null, Alignment.Trailing)
    //    val lbSub   = new Label("Sub title:"      , null, Alignment.Trailing)
    val lbTime  = new Label("Time signatures:", null, Alignment.Trailing)
    val lbTuplet= new Label("Tuplet brackets:", null, Alignment.Trailing)
    val lbMIDI  = new Label("MIDI files:"     , null, Alignment.Trailing)

    //    val ggTitle = new TextField("Title", 12)
    //    val ggSub   = new TextField("Sub title", 12)
    val ggTime  = new ComboBox(Seq("Raw Rationals", "Rounded Rationals", "Decimals"))
    val ggTuplet= new CheckBox
    val ggMIDI  = new CheckBox

    ggTuplet.selected        = true
    ggMIDI .selected        = true
    ggTime.selection.index  = LilyTimeSignature.Raw.id

    import Springs._
    val pane    = new SpringPanel {
      contents ++= Seq(/* lbTitle, lbSub, */ lbTime, lbTuplet, lbMIDI, /* ggTitle, ggSub, */ ggTime, ggTuplet, ggMIDI)
      linkHeight(/* lbTitle, lbSub, */ lbTime, lbTuplet, lbMIDI,
                 /* ggTitle, ggSub, */ ggTime, ggTuplet, ggMIDI)
      linkWidth (/* lbTitle, lbSub, */ lbTime, lbTuplet, lbMIDI)
      // linkWidth (ggTitle, ggSub, ggTime)
      cons(lbTime /* lbTitle */ ).x = 4
      cons(lbTime /* lbTitle */ ).y = 4
      cons(ggTime /* ggTitle */).y = 4
      vseq(/* lbTitle, lbSub, */ lbTime, lbTuplet, lbMIDI)
      vseq(/* ggTitle, ggSub, */ ggTime, ggTuplet, ggMIDI)
      //      cons(ggTitle ).x  = cons(lbTitle ).right  + 4
      //      cons(ggSub   ).x  = cons(lbSub   ).right  + 4
      cons(ggTime  ).x  = cons(lbTime  ).right  + 4
      cons(ggTuplet).x  = cons(lbTuplet).right  + 4
      cons(ggMIDI  ).x  = cons(lbMIDI  ).right  + 4
      cons(this).right  = cons(/* ggTitle */ ggTime ).right  + 4
      cons(this).bottom = cons(ggMIDI  ).bottom + 4
    }

    val opt = OptionPane.apply(
      message = pane, optionType = OptionPane.Options.OkCancel, messageType = OptionPane.Message.Plain)
    val res = opt.show(parent)
    if (res == OptionPane.Result.Ok) {
      val dlg = FileDialog.save(init = Some(defaultFile()), title = "Export As Lilypond Score")
      val res = dlg.show(parent)
      res.foreach { f =>
        ExportLilypond(info = info, genome = genome, out = f,
          timeSig = LilyTimeSignature(ggTime.selection.index), tupletBrackets = ggTuplet.selected,
          midi = ggMIDI.selected)
      }
    }
  }

  private def defaultFile(): File = {
    val desktop = userHome / "Desktop"
    val dir     = if (desktop.canWrite) desktop else userHome

    @tailrec def loop(i: Int): File = {
      val test = dir / s"out${if (i == 0) "" else (i+1).toString}.pdf"
      if (test.exists()) loop(i + 1) else test
    }

    loop(0)
  }

  /** Exports a number of chromosomes as Lilypond score and (optionally) MIDI.
    *
    * @param genome   the sequence of chromosomes
    * @param out      the file (extension will be stripped!)
    * @param midi     whether to output to MIDI file as well or not
    */
  def apply(info: HeaderInfo, genome: GenomeVal, out: File,
            timeSig: LilyTimeSignature = LilyTimeSignature.Raw, tupletBrackets: Boolean = true,
            midi: Boolean = true) {

    import info.{title, subtitle, iterations}

    // ---- lilypond test output ----
    val header = raw"""\header {
      |  title = \markup { \fontsize #-1 \sans { $title }}
      |  tagline = ""
      |  subtitle = \markup { \sans { $subtitle (it = $iterations) }}
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
      |    % \override TupletBracket #'padding = #2
      |    % % allow tuplet bracket to always be visible, even for short tuplets.
      |    % \override TupletBracket #'springs-and-rods = #ly:spanner::set-spacing-rods
      |    % \override TupletBracket #'minimum-length = #3
      |	   \override TupletNumber #'text = #tuplet-number::calc-fraction-text
      |
      |	   % \remove Bar_number_engraver
      |    ${if (timeSig == LilyTimeSignature.Raw) "% " else ""}\override TimeSignature #'X-extent = #'(0 . 5)
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
      |    \override MetronomeMark #'padding = #4
      |
      |    % non-proportional settings:
      |    \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 32)
      |  }
      |  \context { \Voice
      |    \remove "Forbid_line_break_engraver"
      |    ${if (!tupletBrackets) "\\remove \"Tuplet_engraver\"" else ""}
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
      val inner = sq.map { cell =>
        cell.toLilypondString(timeSig = Some(timeSig), annotation = s"#${cell.id+1}")
      } .mkString("\n")

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

    // val jsonPath  = lyf.replaceExt("json")
    // SettingsIO.write(settings, jsonPath)

    Seq(pdfViewer, lyf.replaceExt("pdf").path).!
  }
}