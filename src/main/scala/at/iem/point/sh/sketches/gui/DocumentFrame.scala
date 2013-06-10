package at.iem.point.sh.sketches.gui

import scala.swing.{FlowPanel, Orientation, Swing, BoxPanel, BorderPanel, ScrollPane, Button}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import javax.swing.{Icon, SpinnerNumberModel}
import de.sciss.treetable.{AbstractTreeModel, TreeColumnModel, TreeTable, TreeTableCellRenderer, j}
import java.awt.{Graphics, Graphics2D}
import at.iem.point.sh.sketches.Fitness
import collection.immutable.{IndexedSeq => Vec}
import spire.math.Rational
import de.sciss.swingplus.Spinner
import de.sciss.treetable.j.DefaultTreeTableSorter
import at.iem.point.sh.sketches.genetic.{Selection, Evaluation}
import scala.swing.event.ValueChanged

object DocumentFrame {
  final class Node(val index: Int, val chromosome: Fitness.Chromosome, var fitness: Double = Double.NaN,
                   var selected: Boolean = false, val children: Vec[Node] = Vec.empty)
}
final class DocumentFrame(val document: Document) {
  import DocumentFrame._

  var random      = Fitness.rng(0L)

  val mDur        = new SpinnerNumberModel(8, 1, 128, 1)
  val ggDur       = new Spinner(mDur)
  val mSeed       = new SpinnerNumberModel(0L, 0L, Long.MaxValue, 1L)
  val ggSeed      = new Spinner(mSeed) {
    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
        random = Fitness.rng(mSeed.getNumber.longValue())
    }
  }
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
  //                                       index            fitness selected
  type ColM = TreeColumnModel.Tuple4[Node, Int, Chromosome, Double, Boolean]

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
    def isEditable(node: Node) = false  // could be...
  }

  val selCol    = new TreeColumnModel.Column[Node, Boolean]("Selected") {
    def apply     (node: Node): Boolean = node.selected
    def update    (node: Node, value: Boolean) {}
    def isEditable(node: Node) = false  // could be...
  }

  val tcm = new ColM(seqCol, chromoCol, fitCol, selCol) {
    def getParent(node: Node) = None
  }

  def suckerrrrrrrrrrrrs() {
    val tabcm = tt.peer.getColumnModel
    tabcm.getColumn(0).setPreferredWidth( 48)
    tabcm.getColumn(1).setPreferredWidth(512)
    tabcm.getColumn(2).setPreferredWidth( 72)
    tabcm.getColumn(3).setPreferredWidth( 56) // XXX TODO: should be rendered as checkbox not string
  }

  object tm extends AbstractTreeModel[Node] {
    var root = new Node(index = -1, chromosome = Vec.empty)

    def getChildCount(parent: Node            ): Int  = parent.children.size
    def getChild     (parent: Node, index: Int): Node = parent.children(index)

    def isLeaf(node: Node): Boolean = getChildCount(node) == 0

    def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {}

    def getIndexOfChild(parent: Node, child: Node): Int = parent.children.indexOf(child)

    def getParent(node: Node) = if (node == root) None else Some(root)

    def updateNodes(nodes: Vec[Node]) {
      // val old = root.children
      // root.children = Vec.empty
      // fireNodesRemoved(old: _*)
      // root.children = nodes
      root = new Node(index = -1, chromosome = Vec.empty, children = nodes)
      // fireNodesInserted(nodes: _*)
      fireStructureChanged(root)
      suckerrrrrrrrrrrrs()
      // fireRootChanged()
    }

    def refreshNodes() {
      fireNodesChanged(root.children: _*)
    }
  }

  val tt                  = new TreeTable[Node, ColM](tm, tcm)
  tt.rootVisible          = false
  tt.autoCreateRowSorter  = true
  val dtts = tt.peer.getRowSorter.asInstanceOf[DefaultTreeTableSorter[_, _, _]]
  dtts.setSortsOnUpdates(true)

    // val dtts = new DefaultTreeTableSorter(tm.pee, tcm.peer)
  //  // tt.peer.setRowSorter(dtts)
  //  println(s"Sortable(0)? ${dtts.isSortable(0)}; Sortable(2)? ${dtts.isSortable(2)}")
  //  dtts.setSortable(0, true)
  //  dtts.setSortable(2, true)

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
  tt.peer.setDefaultRenderer(classOf[Boolean]   , TreeTableCellRenderer.Default.peer)
  suckerrrrrrrrrrrrs()

  val ggScroll = new ScrollPane(tt)

  val pTop = new BoxPanel(Orientation.Vertical) {
    contents += pGen
    contents += Swing.VStrut(4)
    // contents += ggGen
  }

  var evaluation: Evaluation = Evaluation.Windowed()
  var selection : Selection  = Selection .Roulette()

  val pBottom = new FlowPanel {
    contents += new BoxPanel(Orientation.Horizontal) {
      val ggGen = Button("Generate") {
        implicit val r  = random
        val pop         = mPop.getNumber.intValue()
        val dur         = Rational(mDur.getNumber.intValue(), 4)
        val nodes       = Vector.tabulate(pop) { idx =>
          val sq  = Fitness.randomSequence(dur)
          new Node(index = idx, chromosome = sq)
        }
        tm.updateNodes(nodes)
      }
      ggGen.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggGen.peer.putClientProperty("JButton.segmentPosition", "only")

      val ggEval = Button("Evaluate") {
        // println("Bang!")
        val genome  = tm.root.children
        val fun     = evaluation
        genome.foreach { node =>
          node.fitness = fun(node.chromosome)
        }
        tm.refreshNodes()
      }
      ggEval.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggEval.peer.putClientProperty("JButton.segmentPosition", "first")
      val ggEvalSettings = Button("Settings") {
        val ef = new EvaluationSettingsFrame(evaluation)
        ef.view.cell.addListener {
          case value => evaluation = value
        }
      }
      ggEvalSettings.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggEvalSettings.peer.putClientProperty("JButton.segmentPosition", "last")

      val ggSel = Button("Selection") {
        // println("Bang!")
        val genome    = tm.root.children
        val fun       = selection
        val selected  = fun(genome.map(node => (node.chromosome, node.fitness)), random).toSet
        genome.foreach { node =>
          node.selected = selected.contains(node.chromosome)
        }
        tm.refreshNodes()
      }
      ggSel.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggSel.peer.putClientProperty("JButton.segmentPosition", "first")
      val ggSelSettings = Button("Settings") {
        val sf = new SelectionSettingsFrame(selection)
        sf.view.cell.addListener {
          case value => selection = value
        }
      }
      ggSelSettings.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggSelSettings.peer.putClientProperty("JButton.segmentPosition", "last")

      contents ++= Seq(ggGen, Swing.HStrut(8), ggEval, ggEvalSettings, Swing.HStrut(8), ggSel, ggSelSettings)
    }
  }

  new WindowImpl {
    def handler = GeneticApp.windowHandler
    protected def style = Window.Regular

    contents = new BorderPanel {
      add(pTop    , BorderPanel.Position.North )
      add(ggScroll, BorderPanel.Position.Center)
      add(pBottom , BorderPanel.Position.South )
    }
    pack()
    front()
  }
}