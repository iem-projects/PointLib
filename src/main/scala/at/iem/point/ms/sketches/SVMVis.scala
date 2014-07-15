/*
 *  SVMVis.scala
 *  (PointLib - ms)
 *
 *  Copyright (c) 2013-2014 IEM Graz / Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package at.iem.point.ms.sketches

import scala.swing.event.{ButtonClicked, MousePressed, EditDone}
import scala.swing.{ProgressBar, Graphics2D, Swing, Component, FlowPanel, BorderPanel, TextField, Button}
import libsvm.{svm, svm_model, svm_node, svm_problem, svm_parameter}
import Swing._
import java.awt.image.BufferedImage
import scala.concurrent.{ExecutionContext, Future, blocking}
import java.awt.{Graphics, Color}
import javax.swing.Icon
import java.awt
import scala.collection.breakOut

/** Improved Scala version of the svm_toy */
object SVMVis {
  private final val DEFAULT_PARAM = "-t 2 -c 100"
  private final val colors        = Array[Color](
    // new Color(  0,   0,   0), 
    new Color(  0, 120, 120), 
    new Color(120, 120,   0), 
    new Color(120,   0, 120), 
    new Color(  0, 200, 200), 
    new Color(200, 200,   0), 
    new Color(200,   0, 200)
  )
}

class SVMVis(rows: Int = 400, columns: Int = 400) extends BorderPanel {
  import SVMVis._

  private var points        = Vec.empty[Point]
  private var currentLabel  = 0

  private val ggProg = new ProgressBar

  private val categIcon = new Icon {
    def getIconHeight: Int = 16
    def getIconWidth : Int = 16

    def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      g.setColor(colors(currentLabel + 3))
      g.fillRect(x, y, 16, 16)
      g.setColor(Color.black)
      g.drawRect(x, y, 15, 15)
    }
  }

  private val ggInputLine: TextField = new TextField(DEFAULT_PARAM, 16) {
    listenTo(this)
    var lastAna = text
    reactions += {
      case EditDone(_) =>
        if (lastAna != text) {
          lastAna = text
          analyze()
        }
    }
  }
  private val butChange: Button = new Button("Change") {
    icon = categIcon
    listenTo(this)
    reactions += {
      case ButtonClicked(_) =>
        buttonChangeClicked()
        repaint()   // new icon color
    }
  }

  private val butRun: Button = Button("Run") {
    analyze()
  }
  private val butClear = Button("Clear") {
    buttonClearClicked()
  }

  private val p = new FlowPanel(butChange, butRun, butClear, ggInputLine)
  add(ggProg, BorderPanel.Position.North )
  add(View  , BorderPanel.Position.Center)
  add(p     , BorderPanel.Position.South )

  private def drawPoint(g: Graphics2D, p: Point): Unit = {
    val c = colors(p.label + 3)
    g.setColor(c)
    g.fillRect((p.x * columns).toInt, (p.y * rows).toInt, 4, 4)
  }

  private def clearAll(): Unit = {
    points = Vec.empty
    withGraphics { g =>
      g.setColor(Color.black)
      g.fillRect(0, 0, columns, rows)
    }
  }

  private def withGraphics[A](fun: Graphics2D => A): A = {
    val g   = buffer.createGraphics()
    val res = fun(g)
    g.dispose()
    View.repaint()
    res
  }

  //  private def drawAllPoints(): Unit = withGraphics { g =>
  //    points.foreach(drawPoint(g, _))
  //  }

  private def buttonChangeClicked(): Unit =
    currentLabel = (currentLabel + 1) % 3

  def paramText         : String        = ggInputLine.text
  def paramText_= (value: String): Unit = ggInputLine.text = value

  def parameters(numFeatures: Int, args: String = paramText): svm_parameter = {
    val param         = new svm_parameter
    param.svm_type    = svm_parameter.C_SVC
    param.kernel_type = svm_parameter.RBF
    param.degree      = 3
    param.gamma       = 0
    param.coef0       = 0
    param.nu          = 0.5
    param.cache_size  = 40
    param.C           = 1
    param.eps         = 1e-3
    param.p           = 0.1
    param.shrinking   = 1
    param.probability = 0
    param.nr_weight   = 0
    param.weight_label= new Array[Int   ](0)
    param.weight      = new Array[Double](0)

    val argsV         = args.trim().split(" \t\n\r\f".toArray)
    val argsNum       = argsV.size
    var argI          = 0

    def checkArgIdx() = require(argI < argsNum)

    def popString(): String = {
      checkArgIdx()
      val res = argsV(argI)
      argI += 1
      res
    }

    def popInt(): Int = {
      val s = popString()
      try Integer.parseInt(s) catch { case _: NumberFormatException =>
        sys.error(s"Argument value must be an integer: $s")
      }
    }

    def popDouble(): Double = {
      val s = popString()
      try java.lang.Double.valueOf(s) catch { case _: NumberFormatException =>
        sys.error(s"Argument value must be a decimal number: $s")
      }
    }

    while (argI < argsNum) {
      val s = popString()
      require(s.length == 2 && s.charAt(0) == '-', s"Arguments must be '-' followed by one character: $s")

      s.charAt(1) match {
        case 's' => param.svm_type    = popInt()
        case 't' => param.kernel_type = popInt()
        case 'd' => param.degree      = popInt()
        case 'g' => param.gamma       = popDouble()
        case 'r' => param.coef0       = popDouble()
        case 'n' => param.nu          = popDouble()
        case 'm' => param.cache_size  = popDouble()
        case 'c' => param.C           = popDouble()
        case 'e' => param.eps         = popDouble()
        case 'p' => param.p           = popDouble()
        case 'h' => param.shrinking   = popInt()
        case 'b' => param.probability = popInt()
        case 'w' =>
          param.nr_weight     += 1
          param.weight_label :+= popInt()    // yo crazy mama, luv it, implicits. note: we use next independent arg
          param.weight       :+= popDouble()

        case other =>
          Console.err.print(s"Unknown option: $other")
      }
    }

    if (param.gamma <= 0) {
      if (param.kernel_type == svm_parameter.PRECOMPUTED) {
        // nada
      } else if (param.svm_type == svm_parameter.EPSILON_SVR || param.svm_type == svm_parameter.NU_SVR) {
        param.gamma = 1
      } else {
        val base = if (param.gamma == 0.0) 1.0 else -param.gamma
        param.gamma = base / numFeatures    // e.g. -2 becomes 2 / numFeatures
      }
    }

    param
  }

  def mkProblem[A](in: Vec[A])(label: A => Double)(features: A => Vec[Double]): svm_problem = {
    val prob  = new svm_problem
    val sz    = in.size
    prob.l    = sz
    prob.y    = in.map(label)(breakOut)
    prob.x    = in.map { x =>
      val vec = features(x)
      vec.iterator.zipWithIndex.map { case (value, idx) =>
        val n = new svm_node
        n.index = idx + 1
        n.value = value
        n
      } .toArray // (breakOut) : Array[svm_node]
    } (breakOut)
    prob
  }

  def train(prob: svm_problem, param: svm_parameter): svm_model = svm.svm_train(prob, param)

  // NOTE: the prob's labels are not used
  def predict(model: svm_model, prob: svm_problem): Vec[Int] =
    (prob.y.iterator zip prob.x.iterator).map { case (label, x) =>
      val d: Double = svm.svm_predict(model, x)
      val categ = d.toInt
      categ // == label
    } .toIndexedSeq // (breakOut)

  def verify(model: svm_model, prob: svm_problem): (Vec[Boolean], Int, Double) = {
    val pred = predict(model, prob)
    val vec: Vec[Boolean] = (pred.iterator zip prob.y.iterator).map { case (p1, target) =>
      p1 == target
    } .toIndexedSeq // (breakOut)
    val abs = vec.count(identity)
    val rel = abs.toDouble / pred.size
    (vec, abs, rel)
  }

  private def analyze(): Unit = {
    if (points.isEmpty) return

    val param = parameters(2)

    if (param.kernel_type == svm_parameter.PRECOMPUTED) {

    } else if (param.svm_type == svm_parameter.EPSILON_SVR || param.svm_type == svm_parameter.NU_SVR) {
      val prob: svm_problem = ???
      prob.x = Array.ofDim[svm_node](prob.l, 1)

      points.zipWithIndex.foreach { case (pt, i) =>
        prob.x(i)(0)        = new svm_node
        prob.x(i)(0).index  = 1
        prob.x(i)(0).value  = pt.x
        prob.y(i)           = pt.y
      }

      val model     = svm.svm_train(prob, param)
      val x         = new Array[svm_node](1)
      x(0)          = new svm_node
      x(0).index    = 1
      val j         = new Array[Int](columns)

      ???
//      {
//        var i: Int = 0
//        while (i < width) {
//          {
//            x(0).value = i.asInstanceOf[Double] / width
//            j(i) = (height * svm.svm_predict(model, x)).asInstanceOf[Int]
//          }
//          ({
//            i += 1; i - 1
//          })
//        }
//      }
//      buffer_gc.setColor(colors(0))
//      buffer_gc.drawLine(0, 0, 0, height - 1)
//      window_gc.setColor(colors(0))
//      window_gc.drawLine(0, 0, 0, height - 1)
//      val p = (param.p * height).toInt
//
//      {
//        var i: Int = 1
//        while (i < width) {
//          {
//            buffer_gc.setColor(colors(0))
//            buffer_gc.drawLine(i, 0, i, height - 1)
//            window_gc.setColor(colors(0))
//            window_gc.drawLine(i, 0, i, height - 1)
//            buffer_gc.setColor(colors(5))
//            window_gc.setColor(colors(5))
//            buffer_gc.drawLine(i - 1, j(i - 1), i, j(i))
//            window_gc.drawLine(i - 1, j(i - 1), i, j(i))
//            if (param.svm_type == svm_parameter.EPSILON_SVR) {
//              buffer_gc.setColor(colors(2))
//              window_gc.setColor(colors(2))
//              buffer_gc.drawLine(i - 1, j(i - 1) + p, i, j(i) + p)
//              window_gc.drawLine(i - 1, j(i - 1) + p, i, j(i) + p)
//              buffer_gc.setColor(colors(2))
//              window_gc.setColor(colors(2))
//              buffer_gc.drawLine(i - 1, j(i - 1) - p, i, j(i) - p)
//              window_gc.drawLine(i - 1, j(i - 1) - p, i, j(i) - p)
//            }
//          }
//          ({
//            i += 1; i - 1
//          })
//        }
//      }
    }
    else {
      val prob    = mkProblem(points)(_.label)(p => Vec(p.x, p.y))
      val model   = train(prob, param)

      //      var correct: Int = 0
      //      for (p <- points) {
      //        x(0).value = p.x
      //        x(1).value = p.y
      //        val d: Double = svm.svm_predict(model, x)
      //        val categ = d.toInt
      //        if (categ == p.label) correct += 1
      //      }
      val (_, numCorrect, percentCorrect) = verify(model, prob)

      println(s"$numCorrect out of ${points.size} predictions were correct (${(percentCorrect * 100).toInt}%).")

      val x       = new Array[svm_node](2)
      x(0)        = new svm_node
      x(1)        = new svm_node
      x(0).index  = 1
      x(1).index  = 2

      import ExecutionContext.Implicits.global
      ggProg.value = 0

      val data = Future {
        blocking {
          (0 to columns).map { i =>
            val p = i * 100 / columns
            Swing.onEDT(ggProg.value = p)

            (0 to rows).map { j =>
              x(0).value  = i.toDouble / columns
              x(1).value  = j.toDouble / rows
              val d       = svm.svm_predict(model, x)
              if (param.svm_type == svm_parameter.ONE_CLASS && d < 0) 2 else d
            }
          }
        }
      }

      data.foreach { ds =>
        Swing.onEDT {
          withGraphics { g =>
            for {
              i <- 0 to columns
              j <- 0 to rows
            } {
              val d = ds(i)(j) // ds(i * rows + j)
              g.setColor(colors(d.toInt))
              g.drawLine(i, j, i, j)
            }
            points.foreach(drawPoint(g, _))
          }
        }
      }
    }
  }

  private def buttonClearClicked(): Unit = clearAll()

  private val buffer: BufferedImage = {
    val res = new BufferedImage(columns, rows, BufferedImage.TYPE_INT_ARGB)
    val g = res.createGraphics()
    g.setColor(Color.black)
    g.fillRect(0, 0, columns, rows)
    g.dispose()
    res
  }

  private object View extends Component {
    preferredSize = (columns + 1, rows + 1)

    override protected def paintComponent(g: Graphics2D): Unit =
      g.drawImage(buffer, 0, 0, peer)

    listenTo(mouse.clicks)
    reactions += {
      case MousePressed(_, pt, _, _, _) if pt.x >= 0 && pt.x <= columns && pt.y >= 0 && pt.y <= rows =>
        val p = Point(pt.x.toDouble / columns, pt.y.toDouble / rows, currentLabel)
        points :+= p
        withGraphics(drawPoint(_, p))
    }
  }

  def addPoint(x: Double, y: Double, label: Int): Unit = {
    val p = Point(x = x, y = y, label = label)
    points :+= p
    withGraphics(drawPoint(_, p))
  }

  def clearPoints() = clearAll()

  private case class Point(x: Double, y: Double, label: Int)
}