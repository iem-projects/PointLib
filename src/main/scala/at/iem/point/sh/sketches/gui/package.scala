package at.iem.point.sh.sketches

import scala.annotation.{tailrec, switch}
import javax.swing.WindowConstants
import scalaswingcontrib.group.GroupPanel
import scala.swing.{Button, BoxPanel, Orientation, Alignment, Label, Component, UIElement, Swing}
import Swing._
import language.reflectiveCalls

package object gui {
  private object GroupPanelInterpolation {
    def mkPanel(comp: Vector[Vector[Component]]): GroupPanel = {
      //      println(comp.map(_.map {
      //        case lb: Label  => lb.text
      //        case b : Button => b.text
      //        case other      => other.getClass.getName
      //      }))
      // val numRows = comp.size
      val numCols = comp.headOption.map(_.size).getOrElse(0)
      new GroupPanel {
        override type InParallel    = InGroup[javax.swing.GroupLayout#ParallelGroup  ]
        override type InSequential  = InGroup[javax.swing.GroupLayout#SequentialGroup]
        theHorizontalLayout is Sequential((0 until numCols).map(i =>
          Parallel(comp.map(_.apply(i): InParallel): _*): InSequential): _*)
        theVerticalLayout   is Sequential(comp.map(row =>
          Parallel(Baseline)(row.map(c => c: InParallel): _*): InSequential): _*)
      }
    }

    def mkCell(open: Vector[Component]): Component = open match {
      case Vector()       => Swing.HGlue
      case Vector(single) => single
      case _ =>
        new BoxPanel(Orientation.Horizontal) {
          open.zipWithIndex.foreach { case (_c, i) =>
            if (i > 0) contents += Swing.HStrut(8)
            contents += _c
          }
        }
    }

    def debug() {
      println("--")
    }
  }
  implicit final class GroupPanelInterpolation(val sc: StringContext) /* extends AnyVal */ {
    import GroupPanelInterpolation._

    def form(args: Any*): GroupPanel = {
      var col     = 0
      var row     = 0
      // var width   = 0
      // var height  = 0

      var comp    = Vector.empty[Vector[Component]]
      var open    = Vector.empty[Component]

      def addComponent(c: Component) {
        open :+= c
      }

      def addCellPart(s: String) {
        if (s.isEmpty) return
        val st = s.trim
        val c = if (st.isEmpty) {
          Swing.HGlue
        } else {
          val lead  = s.indexOf(st)
          val trail = s.length - (lead + st.length)
          val align = if (trail > lead) Alignment.Trailing else Alignment.Leading
          new Label(st, Swing.EmptyIcon, align)
        }
        addComponent(c)
      }

      //      val st = s.trim
      //      val c  = if (st.isEmpty) {
      //        Swing.HGlue
      //      } else {
      //        val lead  = s.indexOf(st)
      //        val trail = s.length - (lead + st.length)
      //        val align = if (trail > lead) Alignment.Trailing else Alignment.Leading
      //        new Label(st, Swing.EmptyIcon, align)
      //      }
      //      addComponent(c)

      //      comp = if (row == comp.size) comp :+ Vector(c) else {
      //        comp.updated(row, comp(row) :+ c)
      //      }
      //      col += 1

      def newLine() {
        // println("newLine()")
        flushCell()
        row += 1
        col  = 0
      }

      def flushCell() {
        val c = mkCell(open)
        // println(s"flushCell($c)")
        comp = if (row == comp.size) comp :+ Vector(c) else {
          comp.updated(row, comp(row) :+ c)
        }
        col += 1
        open = Vector.empty
      }

      def addLinePart(s: String) {
        // println(s"""addLinePart("$s"""")
        val i = s.indexOf('|')
        if (i < 0) addCellPart(s) else {
          addCellPart(s.substring(0, i))
          flushCell()
          addLinePart(s.substring(i + 1))
        }
      }

      @tailrec def addPart(s: String) {
        // println(s"""addPart("$s"""")
        val i = s.indexOf('\n')
        if (i < 0) addLinePart(s) else {
          addLinePart(s.substring(0, i))
          newLine()
          addPart(s.substring(i + 1).stripMargin)
        }
      }

      def addArg(x: Any) {
        x match {
          case c: Component => addComponent(c)
          case _            => throw new IllegalArgumentException(s"$x is not a swing.Component")
        }
      }

      val res = sc.parts match {
        case head +: tail =>
          addPart(head.stripMargin)
          (args zip tail).foreach { case (arg, part) =>
            // debug()
            addArg (arg )
            addPart(part)
          }
          newLine()
          mkPanel(comp)

        case _ => new GroupPanel
      }
      res
    }
  }
}