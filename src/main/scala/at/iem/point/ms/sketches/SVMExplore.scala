package at.iem.point.ms.sketches

import scala.swing._
import de.sciss.numbers.Implicits._
import java.awt.EventQueue
import scala.swing.event.ButtonClicked

object SVMExplore extends SimpleSwingApplication {
  val problems    = SVM.normalize(SVM.allProblems)
  require(problems.nonEmpty)
  val numFeatures = problems.head.features.size
  require(numFeatures >= 2)
  val featureNames = problems.head.features.map(_.name)

  println(problems.mkString("\n"))

  lazy val applet = new SVMVis()

  def update(xi: Int, yi: Int): Unit = {
    val xi0 = xi.clip(0, numFeatures - 1)
    val yi0 = yi.clip(0, numFeatures - 1)
    applet.clearPoints()
    problems.foreach { p =>
      val x = p.features(xi0)
      val y = p.features(yi0)
      applet.addPoint(x.value, y.value, p.label)
    }
    applet.repaint()
    // applet.runAnalysis()
  }

  lazy val top: Frame = {
    // val width     = 400
    // val height    = width + 50

    def mkFeatureSel() = {
      val radios = Vector.fill(numFeatures)(new RadioButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) => performUpdate()
        }
      })
      new ButtonGroup(radios: _*) -> radios
    }

    lazy val lb = (0 until numFeatures).map(i => new Label(i.toString))
    lazy val (gx, gxr) = mkFeatureSel()
    lazy val (gy, gyr) = mkFeatureSel()

    lazy val pFeat = new GridPanel(4, numFeatures) {
      contents ++= featureNames.map(new Label(_))
      contents ++= lb
      contents ++= gx.buttons
      contents ++= gy.buttons
    }

    def performUpdate(): Unit = {
      for {
        bx <- gx.selected
        by <- gy.selected
      } {
        val xi = gxr.indexOf(bx)
        val yi = gyr.indexOf(by)
        println(s"xi $xi, yi $yi")
        update(xi, yi)
      }
    }

    val pFeat2 = new BorderPanel {
      add(pFeat   , BorderPanel.Position.Center)
      // add(ggUpdate, BorderPanel.Position.East  )
    }

    val f         = new MainFrame {
      title = "SVM Explore, Y = boring, B = promising"
      // peer.getContentPane.add(applet)
      contents = new BorderPanel {
        add(applet, BorderPanel.Position.Center)
        add(pFeat2, BorderPanel.Position.South )
      }
      resizable = false
    }
    f.pack().centerOnScreen()
    f.open()

    applet.repaint()

    EventQueue.invokeLater(new Runnable() {
      def run(): Unit = {
        gx.select(gxr(0))
        gy.select(gyr(1))
        update(0, 1)
      }
    })

    f
  }
}
