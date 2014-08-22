package at.iem.point.er
package sketches

import java.awt.{Color, RenderingHints, Font, Toolkit}
import java.io.File
import com.alee.laf.WebLookAndFeel
import de.sciss.desktop.Menu
import de.sciss.desktop.Window
import de.sciss.sonogram.Overview
import de.sciss.synth
import synth.io.AudioFile
import de.sciss.desktop.impl.{WindowImpl, SwingApplicationImpl}
import de.sciss.desktop._
import scala.swing._
import Swing._

import scala.swing.event.Key

object Main extends SwingApplicationImpl("PointLib") {
  type Document = sketches.Document

  def boot(): Unit = AudioSystem.start()

  private lazy val recent = RecentFiles(userPrefs[List[File]]("recent-files"))(open)

  var pdfFun = (_: DocumentFrame) => ()

  var palette: Overview.Palette = Overview.Palette.Intensity

  lazy val menuFactory: Menu.Root = {
    import Menu._
    import KeyStrokes._
    Root().add(
      Group("file", "File").add(
        Item("open")("Open..." -> (menu1 + Key.O)) {
          openDialog()
        }
      ).add(recent.menu).addLine().add(
        Group("import", "Import").add(
          Item("onsets", "Onsets Settings...")
        )).add(
        Group("export", "Export").add(
          Item("audiofile", proxy("Audio File..." -> (menu1 + Key.S)))
        ).add(
          Item("score", proxy("Score..." -> (menu1 + shift + Key.S)))
        ).add(
          Item("screenshot", "Screenshot As PDF...")
        ).addLine().add(
          Item("onsets", "Onsets Settings...")
        )
      )
    ).add(
      Group("tools", "Tools").add(
        Item("mixer", proxy("Mixer" -> (menu1 + Key.M)))
      ).add(
        Item("pitch", proxy("Pitch Analysis" -> (menu1 + Key.P)))
      ).add(
        Item("onsets", proxy("Onsets Detection" -> (menu1 + Key.D)))
      ).addLine().add(
        Item("onsets-unify", "Onsets Unification")
      )
    )
  }

  override def init(): Unit = {
    WebLookAndFeel.install()
    boot()

    if (!Desktop.isMac) new WindowImpl {
      private val img = Toolkit.getDefaultToolkit.getImage(Main.getClass.getResource("icon.png"))
      def handler: WindowHandler = Main.windowHandler
      title     = "Feature Extraction"
      contents  = new Component {
        preferredSize = (256, 256)
        font = new Font(Font.SANS_SERIF, Font.PLAIN, 20)

        override protected def paintComponent(g: Graphics2D): Unit = {
          g.setRenderingHint(RenderingHints.KEY_RENDERING   , RenderingHints.VALUE_RENDER_QUALITY)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON  )
          g.drawImage(img, 0, 0, 256, 256, peer)
          g.setColor(Color.white)
          val dx = -32f
          g.drawString("PATTERNS", 110f + dx, 101f)
          // g.setColor(Color.gray)
          g.drawString("OF", 110f + 36f + dx, 133f)
          // g.setColor(Color.white)
          g.drawString("INTUITION", 110f + dx, 165f)
        }
      }
      closeOperation = Window.CloseExit
      resizable = false
      pack()
      front()
    }
  }

  def openDialog(): Unit = GUI.openAudioFileDialog().foreach(open)

  def open(f: File): Unit = {
    val fileSpec  = AudioFile.readSpec(f)
    val doc       = new Document(f, fileSpec)
    /* val frame = */ new DocumentFrame(doc)
    recent.add(f)
  }
}
