package at.iem.point.er
package sketches

import java.io.File
import com.alee.laf.WebLookAndFeel
import de.sciss.synth
import synth.io.AudioFile
import de.sciss.desktop.impl.{WindowImpl, SwingApplicationImpl}
import de.sciss.desktop._
import scala.swing.Swing
import Swing._

import scala.swing.event.Key

object Main extends SwingApplicationImpl("PointLib") {
  type Document = sketches.Document

  def boot(): Unit = AudioSystem.start()

  private lazy val recent = RecentFiles(userPrefs[List[File]]("recent-files"))(open)

  var pdfFun = (_: DocumentFrame) => ()

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
      closeOperation = Window.CloseExit

      title = "Patterns of Intuition"
      size  = (160, 160)
      front()

      def handler: WindowHandler = Main.windowHandler
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
