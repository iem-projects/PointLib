package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.desktop.{FileDialog, Menu}
import de.sciss.muta.gui.DocumentFrame
import de.sciss.audiowidgets.Transport
import de.sciss.midi

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
  override def rowHeight = 176 // 128 // 64

  // override def useNimbus          = Desktop.isLinux
  // override def useInternalFrames  = !Desktop.isMac

  protected override def init(): Unit = {
    super.init()
    import Menu._
    val root = menuFactory
    root.add(Group("extra", "Extra")
      .add(Item("screenshot")("Save PDF Screenshot...")(saveScreenshot()))
    )
  }

  def saveScreenshot(): Unit =
    windowHandler.windows.toList.headOption.foreach { w =>
      val dlg = FileDialog.save()
      dlg.show(Some(w)).foreach { file =>
        val view = w.component.contents.head
        de.sciss.pdflitz.Generate(file, view)
      }
    }

  private lazy val sequencer = midi.Sequencer.open()

  def stop(): Unit = if (sequencer.isPlaying) sequencer.stop()

  def play(c: GeneticSystem.Chromosome): Unit = {
    val chords  = c.map(_._1)
    val notes   = chords.zipWithIndex.flatMap { case (chord, idx) =>
      chord.notes.map { n =>
        n.copy(offset = idx * 0.7)
      }
    }
    implicit val ticks = midi.TickRate.tempo(120, 256)
    val events  = notes.flatMap(_.toMIDI(channel = 0))
    val track   = midi.Track(events)
    val seq     = midi.Sequence(Vec(track))

    stop()
    sequencer.play(seq)
  }

  override protected def configureDocumentFrame(frame: DocumentFrame[GeneticSystem.type]): Unit = {
    val strip = Transport.makeButtonStrip(Seq(
      Transport.Stop(stop()),
      Transport.Play(frame.selectedNodes.headOption.map(_.chromosome).foreach(play))
    ))
    frame.topPanel.contents += strip
    // frame.window.size = (800, 800)    // XXX TODO: preferredSize broken
  }
}