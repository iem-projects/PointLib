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
  val labels: Vec[(Int, Int)] = problems.groupBy(_.label).mapValues(_.size).toIndexedSeq.sortBy(_._1)

  // println(problems.mkString("\n"))

  lazy val applet: SVMVis = {
    val res = new SVMVis()
    val ws  = weights.map { case (label, c) => f"-w $label $c%1.2f" } .mkString(" ")
    res.paramText = s"${res.paramText} $ws"
    res
  }

  val ggPercent: TextField = new TextField(3) {
    editable = false
  }

  val weights = genWeights(problems)
  println(s"Weights: $weights")

  def genWeights(p: Vec[SVM.Problem]): Vec[(Int, Double)] = {
    val freq  = p.groupBy(_.label).mapValues(_.size) // .toIndexedSeq.sortWith(_._1)
    val sz    = p.size
    val c0s   = histo.mapValues(num => (sz - num).toDouble / num)
    val min   = histo.valueIterator.min
    c0.map { case (label, c0) => label -> c0 / min } (breakOut)
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
    val (_, rel) = charlie(Vec(xi0, yi0), split = false)
    setPercent(rel)
  }

  def setPercent(p: Double): Unit =
    ggPercent.text = (p * 100).toInt.toString

  /** Runs the whole circuit. It creates a model and predicts the success.
    *
    * @param indices  the indices into the feature vector to use in training and testing
    * @param split    if `false`, uses all problems for training and testing, if `true` does
    *                 `problems.size` iterations, in each of which one problem is excluded
    *                 from the training set and used as sole candidate in the prediction round.
    *                 In this case, the returned success is the cumulative success of these iterations.
    * @return the absolute and relative success
    */
  def charlie(indices: Vec[Int], split: Boolean): (Int, Double, Double) = {
    val numFeat = indices.size
    val param   = applet.parameters(numFeat)

    def parker(tr: Vec[SVM.Problem], ts: Vec[SVM.Problem]): Map[Int, (Int, Int)] = {
      val trP         = applet.mkProblem(tr)(_.label)(p => indices.map(p.features(_).value))
      val tsP         = applet.mkProblem(ts)(_.label)(p => indices.map(p.features(_).value))
      val model       = applet.train(trP, param)
      // val (_, n, _)   = applet.verify(model, tsP)
      val pred        = applet.predict(model,tsP)
      // t1, t2, ...
      // p1, p2, ...

      val m  = Map.empty[Int, (Int, Int)] withDefaultValue (0, 0)
      val m2 = (m /: (tsP zip pred)) { case (m1, (target, p)) =>
        m1 + (target -> { val (total, correct) = m(target); (total + 1, if (target == p) correct + 1 else correct) })
      }
      m2
    }

    val abs = if (split) {
      //      problems.zipWithIndex.count { case (ts, idx) =>
      //        parker(tr = problems.patch(idx, Nil, 1), ts = Vec(ts)) == 1
      //      }
      ???

    } else {
      val m = parker(problems, problems)
      val (tot, abs) = m.values.reduce { case ((tot1, corr1), (tot2, corr2)) => (tot1 + tot2) -> (corr1 + corr2) }
      assert(tot == problems.size)
      val rel = abs.toDouble / tot
      (abs, tot)
    }
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

    lazy val ggPermutSel: Vec[CheckBox] = Vec.fill(numFeatures)(new CheckBox)

    lazy val ggPermutProg: ProgressBar = new ProgressBar {
      labelPainted = true
    }

    lazy val pFeat: GridPanel = new GridPanel(5, numFeatures) {
      contents ++= featureNames.map(new Label(_))
      contents ++= lb
      contents ++= gx.buttons
      contents ++= gy.buttons
      contents ++= ggPermutSel
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

    def selectCombination(indices: Vec[Int]): Unit =
      ggPermutSel.zipWithIndex.foreach { case (gg, i) => gg.selected = indices.contains(i) }

    val ggExcludeTest: CheckBox = new CheckBox("Split-Test")

    lazy val ggRunPermut: Button = Button("Permut") {
      import ExecutionContext.Implicits.global
      libsvm.svm.svm_set_print_string_function(new libsvm.svm_print_interface {
        def print(s: String) = () // shut up
      })
      ggRunPermut.enabled = false
      val excl = ggExcludeTest.selected
      val res = Future {
        var best      = 0.0
        var bestAbs   = 0
        var bestComb  = Vec.empty[Int]
        var num       = 2
        while (num <= numFeatures && best < 1.0) {
          Swing.onEDT {
            ggPermutProg.value  = num * 100 / numFeatures
            ggPermutProg.label  = num.toString
          }
          val xs = (0 until numFeatures).combinations(num)
          xs.foreach { indices =>
            Swing.onEDT(selectCombination(indices))
            val (abs, rel) = blocking(charlie(indices, split = excl))
            if (rel >= best) {
              val pr    = rel > best || indices.size == bestComb.size
              if (rel > best) bestComb = indices // since num grows, only replace if really better not equal
              best      = rel
              bestAbs   = abs
              if (pr) Swing.onEDT {
                setPercent(rel)
                if (rel >= 0.5) printCombination(bestAbs, best, indices)
              }
            }
          }
          num += 1
        }
        (bestAbs, best, bestComb)
      }

      res.foreach { case (bestAbs, best, bestCombi) =>
        Swing.onEDT {
          selectCombination(bestCombi)
          setPercent(best)
          println("---RESULT---")
          printCombination(bestAbs, best, bestCombi)
        }
      }

      res.onComplete { _ =>
        libsvm.svm.svm_set_print_string_function(null)
        Swing.onEDT(ggRunPermut.enabled = true)
      }
    }

    def printCombination(abs: Int, rel: Double, indices: Vec[Int]): Unit = {
      println(indices)
      println(s"$abs out of ${problems.size} (${(rel * 100).toFloat}%)")
    }

    val pFeat2: BorderPanel = new BorderPanel {
      add(pFeat      , BorderPanel.Position.Center)
      add(ggPercent  , BorderPanel.Position.West  )
      add(new BorderPanel {
        add(ggExcludeTest, BorderPanel.Position.North )
        add(ggRunPermut  , BorderPanel.Position.Center)
        add(ggPermutProg , BorderPanel.Position.South )
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
