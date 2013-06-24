package at.iem.point.sh.sketches.gui

import scala.swing.{Action, SplitPane, FlowPanel, Orientation, Swing, BoxPanel, BorderPanel, ScrollPane, Button}
import Swing._
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{FileDialog, Window}
import javax.swing.{Icon, SpinnerNumberModel}
import de.sciss.treetable.{AbstractTreeModel, TreeColumnModel, TreeTable, TreeTableCellRenderer, j}
import java.awt.{EventQueue, Graphics, Graphics2D}
import at.iem.point.sh.sketches.{SettingsIO, ExportLilypond, Fitness}
import collection.immutable.{IndexedSeq => Vec}
import spire.math.Rational
import de.sciss.swingplus.Spinner
import de.sciss.treetable.j.DefaultTreeTableSorter
import at.iem.point.sh.sketches.genetic.{Generation, HeaderInfo, Settings, EvalWindowed, Roulette, Breeding, Selection, Evaluation}
import scala.swing.event.{ButtonClicked, ValueChanged}
import de.sciss.file._
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.Processor
import scala.concurrent.ExecutionContext
import de.sciss.guiflitz.AutoView

object DocumentFrame {
  final class Node(val index: Int, val chromosome: Fitness.Chromosome, var fitness: Double = Double.NaN,
                   var selected: Boolean = false, val children: Vec[Node] = Vec.empty)
}
final class DocumentFrame(val document: Document) { outer =>
  import DocumentFrame._

  var random      = Fitness.rng(0L)
  var evaluation: Evaluation  = EvalWindowed()
  var selection : Selection   = Roulette    ()
  var breeding  : Breeding    = Breeding    ()
  // var generation: Generation  = Generation  ()
  def generation: Generation  = pGen .cell()
  def info      : HeaderInfo  = pInfo.cell()

  val mDur        = new SpinnerNumberModel(16, 1, 128, 1)
  val ggDur       = new Spinner(mDur)
  val mSeed       = new SpinnerNumberModel(0L, 0L, Long.MaxValue, 1L)
  val ggSeed      = new Spinner(mSeed) {
    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
        random = Fitness.rng(mSeed.getNumber.longValue())
    }
  }
  val mPop        = new SpinnerNumberModel(100, 1, 10000, 1)
  val ggPop       = new Spinner(mPop)
  val ggRandSeed  = Button("Rand") {
    mSeed.setValue(util.Random.nextLong()) // System.currentTimeMillis())
  }

  val avCfg       = AutoView.Config()
  avCfg.scroll    = false
  avCfg.small     = true
  val pGen        = AutoView(Generation(), avCfg)
  //    form"""   Duration:|$ggDur |\u2669
  //          |       Seed:|$ggSeed|$ggRandSeed
  //          | Population:|$ggPop |"""
  val pInfo     = AutoView(HeaderInfo(), avCfg)

  import Fitness.Chromosome
  //                                       index            fitness selected
  type ColMTop = TreeColumnModel.Tuple4[Node, Int, Chromosome, Double, Boolean]
  type ColMBot = TreeColumnModel.Tuple2[Node, Int, Chromosome]

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

  val tcmTop = new ColMTop(seqCol, chromoCol, fitCol, selCol) {
    def getParent(node: Node) = None
  }

  val tcmBot = new ColMBot(seqCol, chromoCol) {
    def getParent(node: Node) = None
  }

  def adjustColumns(tt: TreeTable[_, _]) {
    val tabcm = tt.peer.getColumnModel
    val sz    = tabcm.getColumnCount
    tabcm.getColumn(0).setPreferredWidth( 48)
    tabcm.getColumn(0).setMaxWidth      ( 48)
    tabcm.getColumn(1).setPreferredWidth(768)
    if (sz >= 4) {
      tabcm.getColumn(2).setPreferredWidth( 72)
      tabcm.getColumn(2).setMaxWidth      (128)
      tabcm.getColumn(3).setPreferredWidth( 56) // XXX TODO: should be rendered as checkbox not string
      tabcm.getColumn(3).setMaxWidth      ( 56) // XXX TODO: should be rendered as checkbox not string
    }
  }

  abstract class TreeModel extends AbstractTreeModel[Node] {
    var root = new Node(index = -1, chromosome = Vec.empty)

    def getChildCount(parent: Node            ): Int  = parent.children.size
    def getChild     (parent: Node, index: Int): Node = parent.children(index)

    def isLeaf(node: Node): Boolean = getChildCount(node) == 0

    def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {}

    def getIndexOfChild(parent: Node, child: Node): Int = parent.children.indexOf(child)

    def getParent(node: Node) = if (node == root) None else Some(root)

    protected def adjustColumns(): Unit

    def updateNodes(nodes: Vec[Node]) {
      // val old = root.children
      // root.children = Vec.empty
      // fireNodesRemoved(old: _*)
      // root.children = nodes
      root = new Node(index = -1, chromosome = Vec.empty, children = nodes)
      // fireNodesInserted(nodes: _*)
      fireStructureChanged(root)
      adjustColumns()
      // fireRootChanged()
    }

    def refreshNodes() {
      fireNodesChanged(root.children: _*)
    }
  }

  object tmTop extends TreeModel {
    protected def adjustColumns() {
      outer.adjustColumns(ttTop)
    }
  }

  object tmBot extends TreeModel {
    protected def adjustColumns() {
      outer.adjustColumns(ttBot)
    }
  }

  def mkTreeTable[Col <: TreeColumnModel[Node]](tm: TreeModel, tcm: Col): TreeTable[Node, Col] = {
    val tt                  = new TreeTable[Node, Col](tm, tcm)
    tt.rootVisible          = false
    tt.autoCreateRowSorter  = true
    val dtts = tt.peer.getRowSorter.asInstanceOf[DefaultTreeTableSorter[_, _, _]]
    dtts.setSortsOnUpdates(true)
    dtts.setComparator(0, Ordering.Int)
    if (tcm.columnCount >= 4) {
      dtts.setComparator(2, Ordering.Double)
      dtts.setComparator(3, Ordering.Boolean)
    }

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
                val widthDur = duration.toDouble * 1.1
                ChromosomeView.paint(cn, g.asInstanceOf[Graphics2D], getWidth - x, getHeight - y, widthDur)
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

    adjustColumns(tt)
    tt
  }

  val ttTop       = mkTreeTable(tmTop, tcmTop)
  val ggScrollTop = new ScrollPane(ttTop)
  val ttBot       = mkTreeTable(tmBot, tcmBot)
  val ggScrollBot = new ScrollPane(ttBot)

  val pTopSettings = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel(pGen.component, pInfo.component)
    contents += VStrut(4)
    // contents += ggGen
  }

  def settings: Settings = Settings(info, generation, evaluation, selection, breeding)
  def settings_=(s: Settings) {
    evaluation  = s.evaluation
    selection   = s.selection
    breeding    = s.breeding
  }

  def duration = Rational(mDur.getNumber.intValue(), 4)

  def stepEval(genome: Vec[Node]) {
    val fun = evaluation
    var min = Double.MaxValue
    var max = Double.MinValue
    genome.foreach { node =>
      val f = fun(node.chromosome)
      node.fitness = f
      if (f < min) min = f
      if (f > max) max = f
    }
    // normalize
    if (max > min) {
      val off     = -min
      val scale   = 1.0/(max - min)
      genome.foreach { node =>
        node.fitness = (node.fitness + off) * scale
      }
    }
  }

  def stepSelect(genome: Vec[Node]) {
    val fun       = selection
    val selected  = fun(genome.map(node => (node.chromosome, node.fitness)), random).toSet
    genome.foreach { node =>
      node.selected = selected.contains(node.chromosome)
    }
  }

  def stepBreed(genome: Vec[Node]): Vec[Node] = {
    val fun = breeding
    val dur = duration
    val r   = random
    val n   = fun(genome.map(node => (node.chromosome, node.fitness, node.selected)), dur, r)
    n.zipWithIndex.map { case (c, idx) => new Node(index = idx, chromosome = c)}
  }

  def defer(thunk: => Unit) {
    if (EventQueue.isDispatchThread) thunk else onEDT(thunk)
  }

  val pButtons = new FlowPanel {
    contents += new BoxPanel(Orientation.Horizontal) {
      val ggGen = Button("Generate") {
        implicit val r  = random
        val pop         = mPop.getNumber.intValue()
        val dur         = duration
        val nodes       = Vector.tabulate(pop) { idx =>
          val sq  = Fitness.randomSequence(dur)
          new Node(index = idx, chromosome = sq)
        }
        tmTop.updateNodes(nodes)
      }
      ggGen.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggGen.peer.putClientProperty("JButton.segmentPosition", "only")

      val ggEval = Button("Evaluate") {
        stepEval(tmTop.root.children)
        tmTop.refreshNodes()
        ttTop.repaint() // XXX TODO should not be necessary
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

      val ggSel = Button("Select") {
        stepSelect(tmTop.root.children)
        tmTop.refreshNodes()
        ttTop.repaint() // XXX TODO should not be necessary
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

      val ggBreed = Button("Breed") {
        val newNodes = stepBreed(tmTop.root.children)
        tmBot.updateNodes(newNodes)
      }
      ggBreed.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggBreed.peer.putClientProperty("JButton.segmentPosition", "first")
      val ggBreedSettings = Button("Settings") {
        val bf = new BreedingSettingsFrame(breeding)
        bf.view.cell.addListener {
          case value => breeding = value
        }
      }
      ggBreedSettings.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggBreedSettings.peer.putClientProperty("JButton.segmentPosition", "last")

      val ggFeed = Button("\u21E7") {
        tmTop.updateNodes(tmBot.root.children)
      }
      ggFeed.peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
      ggFeed.peer.putClientProperty("JButton.segmentPosition", "only")
      ggFeed.tooltip = "Feed offspring back for next iteration"

      val mNumIter  = new SpinnerNumberModel(10, 1, 10000, 1)
      val ggNumIter = new Spinner(mNumIter)
      ggNumIter.tooltip = "Number of iterations to perform at once"
      val ggIter = new Button("Iterate") { // \u238C \u260D \u267B
        var proc      = Option.empty[Proc]
        val progIcon  = new ProgressIcon(33)

        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            proc match {
              case Some(p) => p.abort()
              case _ =>
                val num = mNumIter.getNumber.intValue()
                val in  = tmTop.root.children
                val p   = new Proc(in, num)
                proc    = Some(p)
                p.addListener {
                  case prog @ Processor.Progress(_, _) => defer {
                    progIcon.value = prog.toInt
                    repaint()
                  }
                }
                import ExecutionContext.Implicits.global
                p.start()
                text            = "\u2716"
                progIcon.value  = 0
                icon            = progIcon
                p.onComplete {
                  case _ => defer {
                    icon  = EmptyIcon
                    text  = "Iterate"
                    proc  = None
                  }
                }
                p.onSuccess {
                  case out => defer {
                    tmTop.updateNodes(out)
                    tmBot.updateNodes(Vec.empty)
                  }
                }
            }
        }

        peer.putClientProperty("JButton.buttonType", "segmentedCapsule")
        peer.putClientProperty("JButton.segmentPosition", "last")
        preferredSize = (72, preferredSize.height)
        minimumSize   = preferredSize
        maximumSize   = preferredSize
      }

      contents ++= Seq(ggGen, HStrut(32), ggEval , ggEvalSettings ,
                              HStrut( 8), ggSel  , ggSelSettings  ,
                              HStrut( 8), ggBreed, ggBreedSettings,
                              HStrut( 8), ggFeed,
                              HStrut( 8), ggNumIter, ggIter)
    }
  }

  val splitTop = new BorderPanel {
    add(pTopSettings, BorderPanel.Position.North )
    add(ggScrollTop , BorderPanel.Position.Center)
    add(pButtons    , BorderPanel.Position.South )
  }

  val splitBot = new BoxPanel(Orientation.Horizontal) {
    contents += ggScrollBot
    contents += HStrut(128 + 7)
  }

  val ggSplit = new SplitPane(Orientation.Horizontal) {
    resizeWeight    = 0.5
    topComponent    = splitTop
    bottomComponent = splitBot
  }

  //  def exportAsLilypond(nodes: Vec[Node], f: File) {
  //    ExportLilypond.dialog(nodes.map(n => (n.chromosome, n.fitness)), f)
  //  }

  new WindowImpl { me =>
    def handler = GeneticApp.windowHandler
    def style   = Window.Regular
    contents    = ggSplit
    title       = "Genetic Algorithm"

    bindMenu("file.export.lily", Action("") {
      val nodes = ttTop.selection.paths.map(_.last).toIndexedSeq.sortBy(-_.fitness)
      if (nodes.nonEmpty) {
        ExportLilypond.dialog(settings, nodes.map(n => (n.chromosome, n.fitness)))
      }
    })
    bindMenu("file.export.settings", Action("") {
      val dlg = FileDialog.save(title = "Export Algorithm Settings")
      dlg.show(Some(me)).foreach { f =>
        SettingsIO.write(settings, f.replaceExt("json"))
      }
    })
    bindMenu("file.import.settings", Action("") {
      val dlg = FileDialog.open(title = "Import Algorithm Settings")
      dlg.show(Some(me)).foreach { f =>
        settings = SettingsIO.read(f)
      }
    })
    pack()
    front()
  }

  class Proc(in: Vec[Node], num: Int) extends ProcessorImpl[Vec[Node], Proc] {
    protected def body(): Vec[Node] = {
      // we want to stop the iteration with evaluation, so that the fitnesses are shown in the top pane
      // ; ensure that initially the nodes have been evaluation
      if (in.exists(_.fitness.isNaN)) stepEval(in)
      checkAborted()
      val out = (in /: (0 until num)) { (itIn, idx) =>
        stepSelect(itIn)
        val itOut = stepBreed(itIn)
        stepEval(itOut)
        val f = (idx + 1).toFloat / num
        progress(f)
        checkAborted()
        itOut
      }
      out
    }
  }
}