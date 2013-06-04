package at.iem.point.sh.sketches

import scala.swing._
import scala.annotation.{tailrec, switch}
import javax.swing.WindowConstants
import scalaswingcontrib.group.GroupPanel

package object gui {
  implicit final class RichFrame(val f: Frame) extends AnyVal {
    def defaultCloseOperation = Frame.CloseOperation(f.peer.getDefaultCloseOperation)
    def defaultCloseOperation_=(value: Frame.CloseOperation) { f.peer.setDefaultCloseOperation(value.id) }
  }

  object Frame {
    object CloseOperation {
      def apply(id: Int): CloseOperation = (id: @switch) match {
        case CloseIgnore .id => CloseIgnore
        case CloseExit   .id => CloseExit
        case CloseHide   .id => CloseHide
        case CloseDispose.id => CloseDispose
      }
    }
    sealed trait CloseOperation { def id: Int }
    case object CloseIgnore  extends CloseOperation { final val id = WindowConstants.DO_NOTHING_ON_CLOSE  }
    case object CloseExit    extends CloseOperation { final val id = WindowConstants.EXIT_ON_CLOSE        }
    case object CloseHide    extends CloseOperation { final val id = WindowConstants.HIDE_ON_CLOSE        }
    case object CloseDispose extends CloseOperation { final val id = WindowConstants.DISPOSE_ON_CLOSE     }
  }
  type Frame = swing.Frame

  implicit final class RichUIElement(val ui: UIElement) extends AnyVal {
    def width : Int = ui.peer.getWidth
    def height: Int = ui.peer.getHeight
  }

  private object GroupPanelInterpolation {
    def mkPanel(comp: Vector[Vector[Component]]): GroupPanel = {
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
  }
  implicit final class GroupPanelInterpolation(val sc: StringContext) extends AnyVal {
    import GroupPanelInterpolation._

    def form(args: Any*): GroupPanel = {
      var col     = 0
      var row     = 0
      // var width   = 0
      // var height  = 0

      var comp    = Vector.empty[Vector[Component]]

      def addCellPart(s: String) {
        val st = s.trim
        val c  = if (st.isEmpty) {
          Swing.HGlue
        } else {
          val lead  = s.indexOf(st)
          val trail = s.length - (lead + st.length)
          val align = if (trail > lead) Alignment.Trailing else Alignment.Leading
          new Label(st, Swing.EmptyIcon, align)
        }
        addComponent(c)
      }

      def newLine() {
        row += 1
        col  = 0
      }

      def addComponent(c: Component) {
        comp = if (row == comp.size) comp :+ Vector(c) else {
          comp.updated(row, comp(row) :+ c)
        }
        col += 1
      }

      def addLinePart(s: String) {
        val i = s.indexOf('|')
        if (i < 0) addCellPart(s) else {
          addCellPart(s.substring(0, i))
          newLine()
          addLinePart(s.substring(i + 1))
        }
      }

      @tailrec def addPart(s: String) {
        val i = s.indexOf('\n')
        if (i < 0) addLinePart(s) else {
          addLinePart(s.substring(0, i))
          addPart(s.substring(i + 1))
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
          addPart(head)
          (tail zip args).foreach { case (part, arg) =>
            addPart(part)
            addArg (arg )
          }
          mkPanel(comp)

        case _ => new GroupPanel
      }
      res
    }
  }
}