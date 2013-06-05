package at.iem.point.sh.sketches.gui

import scala.swing.{FlowPanel, Orientation, Swing, BoxPanel, BorderPanel, ScrollPane, Button}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import javax.swing.{Icon, SpinnerNumberModel}
import de.sciss.treetable.{AbstractTreeModel, TreeModel, TreeColumnModel, TreeTable, TreeTableCellRenderer, j}
import java.awt.{Graphics, Graphics2D}
import at.iem.point.sh.sketches.Fitness
import collection.immutable.{IndexedSeq => Vec}
import spire.math.Rational

object DocumentFrame {
  final case class Node(index: Int, chromosome: Fitness.Chromosome, fitness: Double, children: Vec[Node])
}
final class DocumentFrame(val document: Document) {
  import DocumentFrame._

  val mDur        = new SpinnerNumberModel(8, 1, 128, 1)
  val ggDur       = new Spinner(mDur)
  val mSeed       = new SpinnerNumberModel(0L, 0L, Long.MaxValue, 1L)
  val ggSeed      = new Spinner(mSeed)
  val mPop        = new SpinnerNumberModel(20, 1, 10000, 1)
  val ggPop       = new Spinner(mPop)
  val ggRandSeed  = Button("Rand") {
    mSeed.setValue(util.Random.nextLong()) // System.currentTimeMillis())
  }

  val pGen =
    form"""   Duration:|$ggDur |\u2669
          |       Seed:|$ggSeed|$ggRandSeed
          | Population:|$ggPop |"""

  import Fitness.Chromosome

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

  object tm extends AbstractTreeModel[Node] {
    var root = Node(-1, Vector.empty, 0.0, Vector.empty)

    def getChildCount(parent: Node): Int = parent.children.size
    def getChild     (parent: Node, index: Int): Node = parent.children(index)

    def isLeaf(node: Node): Boolean = getChildCount(node) == 0

    def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {}

    def getIndexOfChild(parent: Node, child: Node): Int = parent.children.indexOf(child)

    def getParent(node: Node) = if (node == root) None else Some(root)

    def refresh(nodes: Vec[Node]) {
      root = Node(-1, Vector.empty, 0.0, nodes)
      fireStructureChanged(root)
      // fireRootChanged()
    }
  }

  val tt          = new TreeTable[Node, ColM](tm, tcm)
  tt.rootVisible  = false
  // tt.expandPath(TreeTable.Path.empty)
  // XXX TODO: working around TreeTable issue #1
  tt.peer.setDefaultRenderer(classOf[Vec[_]], new j.DefaultTreeTableCellRenderer {
    override def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean,
                                                   hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
      super.getTreeTableCellRendererComponent(treeTable, value, selected, hasFocus, row, column)
      value match {
        case c: Chromosome =>
          import Fitness._
          val cn  = c.map(_.normalized)
          val sz  = ChromosomeView.preferredSize(cn)
          setText(null)
          setIcon(new Icon {
            def getIconWidth  = sz.width
            def getIconHeight = sz.height

            def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int) {
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

  val ggScroll = new ScrollPane(tt)

  val ggGen = Button("Generate") {
    implicit val r  = Fitness.rng(mSeed.getNumber.longValue())
    val pop         = mPop.getNumber.intValue()
    val dur         = Rational(mDur.getNumber.intValue(), 4)
    val nodes       = Vector.tabulate(pop) { idx =>
      val sq  = Fitness.randomSequence(dur)
      val f   = 1.0 // XXX TODO
      Node(index = idx, chromosome = sq, fitness = f, children = Vector.empty)
    }
    tm.refresh(nodes)
  }

  val pTop = new BoxPanel(Orientation.Vertical) {
    contents += pGen
    contents += Swing.VStrut(4)
    contents += ggGen
  }

  val pBottom = new FlowPanel {
    contents += Button("Evaluation Settings") {
      new EvaluationSettingsFrame()
    }
  }

  new WindowImpl {
    def handler = GeneticApp.windowHandler
    protected def style = Window.Regular

    contents = new BorderPanel {
      add(pTop    , BorderPanel.Position.North )
      add(ggScroll, BorderPanel.Position.Center)
    }
    pack()
    front()
  }
}