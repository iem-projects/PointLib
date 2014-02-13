package at.iem.point.ms.sketches

import scala.swing._
import de.sciss.numbers.Implicits._
import java.awt.EventQueue
import scala.swing.event.ButtonClicked
import scala.concurrent.{blocking, ExecutionContext, Future}

object SVMExplore extends SimpleSwingApplication {
  val problems: Vec[SVM.Problem] = SVM.normalize(SVM.allProblems)
  require(problems.nonEmpty)
  val numFeatures: Int = problems.head.features.size
  require(numFeatures >= 2)
  val featureNames: Vec[String] = problems.head.features.map(_.name)

  println(problems.mkString("\n"))

  lazy val applet: SVMVis = new SVMVis()

  val ggPercent: TextField = new TextField(3) {
    editable = false
  }

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
    val (_, rel) = charlie(Vec(xi0, yi0))
    setPercent(rel)
  }

  def setPercent(p: Double): Unit =
    ggPercent.text = (p * 100).toInt.toString

  def charlie(indices: Vec[Int]): (Int, Double) = {
    val param = applet.parameters()
    val prob  = applet.mkProblem(problems)(_.label)(p => indices.map(p.features(_).value))
    val model = applet.train(prob, param)
    val (_, abs, rel) = applet.verify(model, prob)
    (abs, rel)
  }

  lazy val top: Frame = {
    // val width     = 400
    // val height    = width + 50

    def mkFeatureSel(): (ButtonGroup, Vec[RadioButton]) = {
      val radios = Vec.fill(numFeatures)(new RadioButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) => performUpdate()
        }
      })
      new ButtonGroup(radios: _*) -> radios
    }

    lazy val lb: Vec[Label] = (0 until numFeatures).map(i => new Label(i.toString))
    lazy val (gx, gxr) = mkFeatureSel()
    lazy val (gy, gyr) = mkFeatureSel()

    lazy val ggPermut: Vec[CheckBox] = Vec.fill(numFeatures)(new CheckBox)

    lazy val ggPermutProg: ProgressBar = new ProgressBar

    lazy val pFeat: GridPanel = new GridPanel(5, numFeatures) {
      contents ++= featureNames.map(new Label(_))
      contents ++= lb
      contents ++= gx.buttons
      contents ++= gy.buttons
      contents ++= ggPermut
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

    def selectCombi(indices: Vec[Int]): Unit =
      ggPermut.zipWithIndex.foreach { case (gg, i) => gg.selected = indices.contains(i) }

    lazy val ggRunPermut: Button = Button("Permut") {
      import ExecutionContext.Implicits.global
      libsvm.svm.svm_set_print_string_function(new libsvm.svm_print_interface {
        def print(s: String) = () // shut up
      })
      val res = Future {
        var best      = 0.0
        var bestAbs   = 0
        var bestCombi = Vec.empty[Int]
        for (num <- 2 to numFeatures) {
          Swing.onEDT {
            ggPermutProg.value = num * 100 / numFeatures
          }
          (0 until numFeatures).combinations(num).foreach { indices =>
            Swing.onEDT(selectCombi(indices))
            val (abs, rel) = blocking(charlie(indices))
            if (rel > best) {
              best      = rel
              bestAbs   = abs
              bestCombi = indices
              Swing.onEDT {
                setPercent(rel)
                if (rel >= 0.9) printCombi(bestAbs, best, bestCombi)
              }
            }
          }
        }
        (bestAbs, best, bestCombi)
      }

      res.foreach { case (bestAbs, best, bestCombi) =>
        Swing.onEDT {
          selectCombi(bestCombi)
          setPercent(best)
          printCombi(bestAbs, best, bestCombi)
        }
      }

      res.onComplete(_ => libsvm.svm.svm_set_print_string_function(null))
    }

    def printCombi(abs: Int, rel: Double, indices: Vec[Int]): Unit = {
      println(indices)
      println(s"$abs out of ${problems.size} (${(rel * 100).toFloat}%)")
    }

    val pFeat2: BorderPanel = new BorderPanel {
      add(pFeat      , BorderPanel.Position.Center)
      add(ggPercent  , BorderPanel.Position.West  )
      add(new BorderPanel {
        add(ggRunPermut , BorderPanel.Position.Center)
        add(ggPermutProg, BorderPanel.Position.South )
      }, BorderPanel.Position.East)
      // add(ggUpdate, BorderPanel.Position.East  )
    }

    val f: MainFrame = new MainFrame {
      title = "SVM Explore, B = boring, Y = promising"
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
