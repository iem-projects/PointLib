package at.iem.point.ot.sketches

import java.awt.{Color, Font, RenderingHints, Toolkit}
import javax.swing.SpinnerNumberModel

import com.alee.laf.WebLookAndFeel
import de.sciss.audiowidgets.Transport
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{Desktop, FileDialog, Window, WindowHandler}
import de.sciss.{midi, muta}
import de.sciss.muta.gui.DocumentFrame
import de.sciss.swingplus.Spinner

import scala.swing.Swing._
import scala.swing.{Component, Graphics2D, Label, Swing}

object GeneticApp extends GeneticAppLike(ManualGeneticSystem)

abstract class GeneticAppLike[A <: GeneticSystem](system: A) extends muta.gui.GeneticApp(system) {
  override def rowHeight = 176 // 128 // 64

  override def useNimbus          = false
  override def useInternalFrames  = false

  lazy val mTimeOut = new SpinnerNumberModel(30, 1, 600, 10)

  protected override def init(): Unit = {
    WebLookAndFeel.install()

    // doesn't work with WebLaF:
    //    super.init()
    //    import Menu._
    //    val root = menuFactory
    //    root.add(Group("extra", "Extra")
    //      .add(Item("screenshot")("Save PDF Screenshot...")(saveScreenshot()))
    //    )

    if (!Desktop.isMac) new WindowImpl {
      private val img = Toolkit.getDefaultToolkit.getImage(GeneticApp.getClass.getResource("icon.png"))
      def handler: WindowHandler = GeneticApp.windowHandler
      title     = "Genetic Algorithm"
      contents  = new Component {
        preferredSize = (256, 256)
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 20)

        override protected def paintComponent(g: Graphics2D): Unit = {
          g.setRenderingHint(RenderingHints.KEY_RENDERING   , RenderingHints.VALUE_RENDER_QUALITY)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON  )
          g.drawImage(img, 0, 0, 256, 256, peer)
          g.setColor(Color.black)
          g.drawString("PATTERNS", 110f, 101f)
          g.setColor(Color.gray)
          g.drawString("OF", 110f, 133f)
          g.setColor(Color.white)
          g.drawString("INTUITION", 110f, 165f)
        }
      }
      closeOperation = Window.CloseExit
      resizable = false
      pack()
      front()
    }
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

  override protected def configureDocumentFrame(frame: DocumentFrame[A]): Unit = {
    val strip = Transport.makeButtonStrip(Seq(
      Transport.Stop(stop()),
      Transport.Play(frame.selectedNodes.headOption.map(_.chromosome).foreach(play))
    ))
    val ggTimeOut = new Spinner(mTimeOut)
    val tp = frame.topPanel
    tp.contents += strip
    tp.contents += Swing.HStrut(8)
    tp.contents += new Label("Timeout [s]:")
    tp.contents += ggTimeOut
    // frame.window.size = (800, 800)    // XXX TODO: preferredSize broken
  }
}