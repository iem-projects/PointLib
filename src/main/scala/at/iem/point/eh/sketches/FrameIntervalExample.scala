package at.iem.point.eh.sketches

import scalax.chart.Charting._
import scala.swing.{MainFrame, Component, GridPanel}
import org.jfree.chart.{ChartPanel, ChartFactory, StandardChartTheme}
import org.jfree.chart.axis.{NumberAxis, NumberTickUnit}
import scalax.chart.XYChart
import at.iem.point.illism._
import de.sciss.pdflitz
import org.jfree.chart.renderer.xy.XYBarRenderer

object FrameIntervalExample extends App {
  sys.props("com.apple.mrj.application.apple.menu.about.name")  = "PointLib"
  sys.props("apple.laf.useScreenMenuBar")                       = "true"
  run(_mode, _allIntervals)

  sealed trait Mode { def filter: Option[Int]; def isImprov: Boolean }
  case class StaticChords(sz: Int) extends Mode { def filter = Some(sz); def isImprov = false }
  case class Improvisation(filter: Option[Int]) extends Mode { def isImprov = true }
  case class Static1(sz: Int, snippets: Int*) extends Mode { def filter = Some(sz); def isImprov = false }

  /** Whether to look at one particular chord type snippet
    * or a free improvisation. In the case of the improv,
    * whether to filter particular chords or include all.
    */
  lazy val _mode: Mode = Improvisation(Some(4)) //  Static1(6, 23) // StaticChords(4)
  /** Whether to just look at the frame intervals (`false`) or to
    * calculate the histogram of all internal intervals (`true`)
    * (these are successive intervals, not all permutations!)
    */
  lazy val _allIntervals  = false
  
  lazy val _sumSnippets   = true

  /** If `true`, include a legend in the plot. */
  lazy val _legend        = false

  /** If `true`, plot relative frequencies, otherwise plot absolute frequencies */
  lazy val _relative      = true

  def run(mode: Mode = _mode, allIntervals: Boolean = _allIntervals, sumSnippets: Boolean = _sumSnippets,
          legend: Boolean = _legend, relative: Boolean = _relative): Unit = defer {
    ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())

    val snippetSet  = mode match {
      case StaticChords (sz) => staticChords(sz)
      case Improvisation(_)  => improvSnippets.last :: Nil
      case Static1      (_, ids @ _*) => ids
    }
    val numSnippets = if (sumSnippets) 1 else snippetSet.size
    val infos0      = snippetSet.map(idx =>
      frameIntervalHisto(snippetIdx = idx, constrainSize = mode.filter, lowTolerance = mode.isImprov,
        allIntervals = allIntervals, legend = legend, relative = relative)
    )
    val infos       = if (sumSnippets && infos0.size > 1) infos0.reduce(sumInfos) :: Nil else infos0
    val maxX        = infos.map(_.histo1.keys.map(_.semitones).max).max + 1 // plus one, because otherwise last bar is shown truncated
    val maxY        = infos.map(_.histo1.values.max).max
    val charts      = infos.map(mkChart)
    charts.foreach { c =>
      setMaxX(c, math.max(if (allIntervals) 12 else 14, maxX))
      setMaxY(c, maxY)
    }
    val numCols     = math.ceil(math.sqrt(numSnippets)).toInt
    val numRows     = (numSnippets + numCols - 1) / numCols

    val panel = new GridPanel(numRows, numCols) {
      vGap  = 24
      hGap  = 24
      contents ++= charts.map { c =>
        val p     = new ChartPanel(c.peer, false)
        val dim   = p.getPreferredSize
        dim.width = dim.width/2
        p.setPreferredSize(dim)
        Component.wrap(p)
      } // useBuffer = false for PDF export
    }

    val intervalTitle = if (allIntervals) "Interval Layer" else "Frame Interval"

    val chordTitle = mode match {
      case StaticChords(sz)         => s"static chords of size $sz"
      case Improvisation(None)      => s"all chords in an improvisation"
      case Improvisation(Some(sz))  => s"chords of size $sz in an improvisation"
      case Static1(sz, _ids @ _*)   => s"static chords of size $sz"
    }

    val frame = new MainFrame {
      title = s"$intervalTitle Histograms for $chordTitle"
      contents = panel
      new pdflitz.SaveAction(panel :: Nil).setupMenu(this)
      pack().centerOnScreen()
    }
    frame.open()
  }
  
  def sumInfos(a: Info, b: Info): Info = {
    require((a.snippets intersect b.snippets).isEmpty)
    require(a.allIntervals == b.allIntervals)
    val sumVoices = (a.voices.toSet ++ b.voices.toSet).toList.sorted
    val sumHisto  = b.histo.foldLeft(a.histo) { case (sum, (key, value)) =>
      sum + (key -> (sum(key) + value))
    }
    Info(a.snippets ++ b.snippets, numChords = a.numChords + b.numChords, histo = sumHisto, voices = sumVoices,
         allIntervals = a.allIntervals, legend = a.legend, relative = a.relative)
  }

  final case class Info(snippets: List[Int], numChords: Int, histo: Map[Interval, Int], voices: List[Int],
                        allIntervals: Boolean, legend: Boolean, relative: Boolean) {

    lazy val histo1: Map[Interval, Double] = if (relative) {
      val mx    = histo.values.max
      val scale = 1.0 / mx
      histo.mapValues(_ * scale)
    } else {
      histo.mapValues(_.toDouble)
    }
  }

  /** Calculates the histogram of the frame intervals in a chord snippet.
    *
    * @param snippetIdx      index of the snippet to load
    * @param constrainSize   whether to filter chords which satisfy a given # of voices or not
    * @return                the calculated information
    */
  def frameIntervalHisto(snippetIdx: Int, constrainSize: Option[Int], allIntervals: Boolean = false,
                         lowTolerance: Boolean = false, legend: Boolean, relative: Boolean): Info = {
    val notes     = loadSnippet(snippetIdx).notes
    val chords0   = ChordUtil.findChords(notes, offsetTolerance = if (lowTolerance) 0.03 else 0.1,
                                                stopTolerance   = if (lowTolerance) 0.5 else 10.0)
    val chords    = constrainSize match {
      case Some(sz) => chords0.filter(_.size == sz)
      case _ => chords0
    }
    val numChords = chords.size
    //    val fi        = chords.map(_.frameInterval)
    val fi0 = if (allIntervals) {
      chords.flatMap(_.layeredIntervals.map(_.modOctave))
    } else {
      chords.map(_.frameInterval /* .modOctave */)
    }

    val fi = if ((constrainSize == Some(3) || constrainSize == Some(4)) && !allIntervals)
      fi0.filter(_.semitones < 16)
    else
      fi0

    val fih       = fi.histogram
    val voices = {
      val set = chords.map(_.size).toSet
      if (set.size == 1) set.head :: Nil else set.toList.sorted
    }
    Info(snippets = snippetIdx :: Nil, numChords = numChords, histo = fih, voices = voices,
      allIntervals = allIntervals, legend = legend, relative = relative)
  }

  def setMaxY(chart: /* CategoryChart */ XYChart, i: Double): Unit = {
    val rangeX = chart.plot.getRangeAxis.asInstanceOf[NumberAxis]
    rangeX.setRange(0.0, i)
  }

  def setMaxX(chart: XYChart, i: Int): Unit = {
    val rangeY = chart.plot.getDomainAxis.asInstanceOf[NumberAxis]
    rangeY.setRange(0.5, i.toDouble - 0.5)
  }

  def mkChart(info: Info): /* CategoryChart */ XYChart = {
    implicit val semitones = (i: Interval) => i.semitones.asInstanceOf[Integer]
    val fihData = info.histo1.toXYSeriesCollection(s"Freq. of ${if (info.allIntervals) "interval layers" else "frame intervals"}")
    //    implicit val comp: Interval => Comparable[Interval] = `this` => new Comparable[Interval] {
    //      def compareTo(that: Interval): Int = `this`.semitones compareTo that.semitones
    //    }
    //    val fihData = info.histo1.toCategoryDataset
    val voicesTxt = info.voices match {
      case single :: Nil => single.toString
      case more => more.mkString("(", ",", ")")
    }
    val snippetsTxt = info.snippets match {
      case single :: Nil => s"snippet #$single"
      case more => more.mkString("snippets ", ",", "")
    }
    val titleOLD = s"Histogram for $snippetsTxt - ${info.numChords} chords of $voicesTxt voices"
    println(titleOLD)
    val title = s"Histogram for $voicesTxt voices"
    val chart   = XYBarChart(fihData, title = title, legend = info.legend)
    // val chart   = BarChart(fihData, title = title, legend = info.legend)
    chart.printableLook()
    val plot    = chart.plot
    // plot.getRenderer.asInstanceOf[BarRenderer].setMaximumBarWidth(0.1)
    plot.getRenderer.asInstanceOf[XYBarRenderer].setMargin(0.0)

    val rangeX  = plot.getDomainAxis.asInstanceOf[NumberAxis]
    // plot.getRenderer.setSeriesPaint(0, Color.darkGray)
    rangeX.setTickUnit(new NumberTickUnit(1))
    // rangeX.setFixedDimension(1)

    val rangeY  = plot.getRangeAxis.asInstanceOf[NumberAxis]
    if (!info.relative) rangeY.setTickUnit(new NumberTickUnit(1))
    // if (info.relative) rangeY.setRange(0, 1)

    chart
  }
}