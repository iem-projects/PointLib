package at.iem.point.sh.sketches

import java.awt.Color
import scalax.chart.XYChart
import scalax.chart.api._
import org.jfree.chart.ChartPanel
import scala.swing.{Component, Frame, SimpleSwingApplication}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import scala.swing.event.WindowClosing
import org.jfree.chart.labels.StandardXYItemLabelGenerator
import de.sciss.pdflitz
import at.iem.point.illism.rhythm.{Ladma, Cell}

object LadmaSets extends SimpleSwingApplication {
  // --- settings ---
  val xaxis: PlotType   = Dilation              // which datum to plot across the x-axis
  val yaxis: PlotType   = Entropy               // which datum to plot across the y-axis
  val source: Source    = FactorsAllForms       // which set of cells forms the source of analysis

  sealed trait PlotType
  sealed trait LadmaType extends PlotType
  case object Mobility extends LadmaType
  case object Tension  extends LadmaType
  case object Entropy  extends LadmaType
  case object TensionByMobility extends LadmaType
  case object Duration extends PlotType
  case object Index    extends PlotType
  case object Dilation extends PlotType

  sealed trait Source
  /** All the base cells without stretching */
  case object BaseForms   extends Source
  /** All the base cells, each stretched with the basic stretch factors */
  case object BaseFactors extends Source
  /** One of the base cells, stretched with the all stretch factors */
  case class  FactorsOneForm(idx: Int) extends Source {
    require(idx >= 0 && idx < cell.size)
  }
  /** All the base cells, each stretched with the all stretch factors */
  case object FactorsAllForms extends Source

  lazy val top = {

    lazy val cells: Vector[Cell] = {
      val seq = source match {
        case BaseForms            => baseCells
        case BaseFactors          => baseCells.flatMap(c => baseFactors.map(c * _))
        case FactorsOneForm(idx)  => factors.map(cell(idx) * _)
        case FactorsAllForms      => baseCells.flatMap(c => factors.map(c * _))
      }
      seq.sortBy(_.dur)
    }

    lazy val m = cells.map(Ladma.mobility)
    lazy val t = cells.map(Ladma.tension)
    lazy val e = cells.map(Ladma.entropy)

    // val data  = m zip t
    // val datas = data.sortBy { case (a, b) => a * b }

    def parse(tpe: PlotType) = tpe match {
      case Mobility           => (m, "Mobility", Some(-0.1))
      case Tension            => (t, "Tension", Some(-0.05))
      case Entropy            => (e, "Entropy", None)
      case TensionByMobility  => ((t zip m).map { case (t0, m0) => math.exp(t0/m0) }, "exp Tension/Mobility", Some(0.9))

      case Duration           => (cells.map(_.dur.toDouble), "Duration", None)
      case Index              => (cells.map(c => (c.id + 1).toDouble), "Cell No.", None)
      case Dilation           => (cells.map(c => (c.dur / baseCells(c.id).dur).toDouble), "Dilation", None)
    }

    // println("-----CELLS-----")
    // cells.map(_.pretty).foreach(println)

    val (xdata, xlabel, xlo) = parse(xaxis)
    val (ydata, ylabel, ylo) = parse(yaxis)

    val data  = xdata zip ydata

    val subtitle = source match {
      case BaseForms            => "Base forms"
      case BaseFactors          => "Cells stretched with base factors"
      case FactorsOneForm(idx)  => s"Cell No. ${idx + 1} stretched with all factors"
      case FactorsAllForms      => "Cells stretched with all factors"
    }

    val labelFun = source match {
      case BaseForms => Some((item: Int) => (item + 1).toString)
      case FactorsOneForm(idx) => Some { (item: Int) =>
        val c = cells(item)
        val b = baseCells(c.id)
        val f = c.dur / b.dur
        f.toString
      }
      case _ => None
    }

    val chart = mkChart(data, ylo = ylo, xlo = xlo, title = "Ladma Measures",
      datalabel = subtitle, xlabel = xlabel, ylabel = ylabel, labelFun = labelFun)
    mkFrame(chart)
  }

  def mkFrame(chart: XYChart): Frame = {
    val panelj  = new ChartPanel(chart.peer, false)
    val panel   = Component.wrap(panelj)
    new Frame {
      contents = panel
      new pdflitz.SaveAction(panel :: Nil).setupMenu(this)
      pack().centerOnScreen()
      listenTo(this)
      reactions += {
        case WindowClosing(_) => quit()
      }
      open()
    }
  }

  def mkChart(data: Iterable[(Double, Double)], ylo: Option[Double], xlo: Option[Double],
              datalabel: String = "Data", xlabel: String = "X", ylabel: String = "y",
              title: String = "Title", labelFun: Option[Int => String]): XYChart = {
    val fihData = data.toXYSeriesCollection(datalabel, autoSort = false)
    val chart   = XYLineChart(fihData, title = title)
    val plot    = chart.plot

    val rangeX  = plot.getDomainAxis // .asInstanceOf[NumberAxis]
    xlo.foreach(rangeX.setLowerBound(_))
    // rangeX.setTickUnit(new NumberTickUnit(1))
    val rangeY  = plot.getRangeAxis // .asInstanceOf[NumberAxis]
    ylo.foreach(rangeY.setLowerBound(_))
    // rangeY.setTickUnit(new NumberTickUnit(1))

    // doesn't do what we wish:
    // plot.getDomainAxis.setLowerMargin(0.05)
    plot.getDomainAxis.setLabel(xlabel)
    plot.getRangeAxis .setLabel(ylabel)

    plot.setBackgroundPaint(Color.white)
    val renderer = new XYLineAndShapeRenderer()
    renderer.setSeriesLinesVisible(0, false)
    renderer.setSeriesShapesVisible(0, true)
    renderer.setSeriesPaint(0, Color.darkGray)

    renderer.setBaseItemLabelsVisible(true)
    labelFun.foreach { fun =>
      renderer.setBaseItemLabelGenerator(new StandardXYItemLabelGenerator() {
        override def generateLabel(dataset: XYDataset, series: Int, item: Int) = fun(item)
      })
    }

    plot.setRenderer(renderer)

    chart
  }
}