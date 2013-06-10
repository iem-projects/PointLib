package at.iem.point.sh.sketches

import scala.swing.{Frame, ScrollPane, SimpleSwingApplication}
import gui._
import spire.syntax._
import de.sciss.treetable._
import collection.immutable.{IndexedSeq => IIdxSeq}

import Fitness.Chromosome
import javax.swing.Icon
import java.awt.{Graphics2D, Graphics, Component}
import de.sciss.swingplus.Implicits._
import de.sciss.swingplus.CloseOperation

object ChromosomeViewTest extends SimpleSwingApplication {
  final case class Node(index: Int, chromosome: Chromosome, fitness: Double, children: IIdxSeq[Node])

  lazy val top = new Frame { f =>
    implicit val rng = Fitness.rng(666L)

    val nodes = Vector.tabulate(5) { idx =>
      val sq  = Fitness.randomSequence(r"12")
      // val cv  = new ChromosomeView(sq)
      // cv
      val f   = 1.0 // XXX TODO
      Node(index = idx, chromosome = sq, fitness = f, children = Vector.empty)
    }

    type ColM = TreeColumnModel.Tuple3[Node, Int, Chromosome, Double]

    val seqCol    = new TreeColumnModel.Column[Node, Int]("Index") {
      def apply     (node: Node): Int = node.index
      def update    (node: Node, value: Int) {}
      def isEditable(node: Node) = false
    }

    val chromoCol = new TreeColumnModel.Column[Node, Chromosome]("Chromosome") {
      def apply     (node: Node): Chromosome = node.chromosome
      def update    (node: Node, value: Chromosome) {}
      def isEditable(node: Node) = false
    }

    val fitCol    = new TreeColumnModel.Column[Node, Double]("Fitness") {
      def apply     (node: Node): Double = node.fitness
      def update    (node: Node, value: Double) {}
      def isEditable(node: Node) = false
    }

    val tcm = new ColM(seqCol, chromoCol, fitCol) {
      def getParent(node: Node) = None
    }

    val tm = new TreeModel[Node] {
      val root = Node(-1, Vector.empty, 0.0, nodes)

      def getChildCount(parent: Node): Int = parent.children.size
      def getChild     (parent: Node, index: Int): Node = parent.children(index)

      def isLeaf(node: Node): Boolean = getChildCount(node) == 0

      def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {}

      def getIndexOfChild(parent: Node, child: Node): Int = parent.children.indexOf(child)
    }

    val tt          = new TreeTable[Node, ColM](tm, tcm)
    tt.rootVisible  = false
    // tt.expandPath(TreeTable.Path.empty)
    // XXX TODO: working around TreeTable issue #1
    tt.peer.setDefaultRenderer(classOf[IIdxSeq[_]], new j.DefaultTreeTableCellRenderer {
      override def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                                     hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
        super.getTreeTableCellRendererComponent(treeTable, value, selected, hasFocus, row, column)
        value match {
          case c: Chromosome =>
            import Fitness._
            val cn  = c.map(_.normalized)
            val sz  = ChromosomeView.preferredSize(cn)
            setIcon(new Icon {
              def getIconWidth  = sz.width
              def getIconHeight = sz.height

              def paintIcon(c: Component, g: Graphics, x: Int, y: Int) {
                g.translate(x, y)
                ChromosomeView.paint(cn, g.asInstanceOf[Graphics2D], getWidth - x, getHeight - y)
                g.translate(-x, -y)
              }
            })
          case _ =>
        }
        this
      }
    })
    tt.peer.setDefaultRenderer(classOf[Int]       , TreeTableCellRenderer.Default.peer)
    tt.peer.setDefaultRenderer(classOf[Double]    , TreeTableCellRenderer.Default.peer)

//    contents = new Component { cmp =>
//      override protected def paintComponent(g: Graphics2D) {
//        cv.paint(g, cmp.width, cmp.height)
//      }
//      preferredSize = cv.preferredSize
//    }

    contents = new ScrollPane(tt)

    this.defaultCloseOperation = CloseOperation.Exit
  }
}