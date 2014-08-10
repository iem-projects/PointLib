package at.iem.point.sh.sketches
package gui

import java.awt.{Color, RenderingHints, Font, Toolkit}

import com.alee.laf.WebLookAndFeel
import de.sciss.desktop.Menu
import de.sciss.desktop.impl.WindowImpl
import de.sciss.muta
import at.iem.point.sh.sketches.genetic.GeneticSystem
import de.sciss.muta.gui.DocumentFrame
import de.sciss.desktop._
import java.awt.event.KeyEvent
import scala.swing._
import scala.swing.event.Key
import Swing._

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
  override protected def useNimbus: Boolean = false
  override protected def useInternalFrames: Boolean = false

  protected override def init(): Unit = {
    WebLookAndFeel.install()

    super.init()
    import Menu._
    import KeyStrokes._
    import KeyEvent._
    val root = menuFactory
    root.get("file.export") match {
      case Some(g: Group) =>
        g.add(
          Item("lily", proxy("Selection As Lilypond Score..." -> (menu1 + shift + Key.S)))
        )
      case _ =>
    }

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

  override protected def configureDocumentFrame(frame: DocumentFrame[GeneticSystem.type]): Unit = {
    frame.bindMenu("file.export.lily", Action("") {
      val genome = frame.selectedNodes.map(n => n.chromosome -> n.fitness)
      ExportLilypond.dialog(info = frame.info, genome = genome, parent = Some(frame.window))
    })

    frame.bindMenu("file.export.table", Action("") {
      val genome = frame.selectedNodes.map(n => n.chromosome -> n.fitness)
      ExportTable.dialog(genome = genome, parent = Some(frame.window))
    })
  }
}