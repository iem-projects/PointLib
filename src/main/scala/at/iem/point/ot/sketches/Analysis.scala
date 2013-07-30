package at.iem.point.ot.sketches

import scala.swing.{Component, ScrollPane, Orientation, BoxPanel, Frame, SimpleSwingApplication}
import at.iem.point.illism._
import scala.swing.event.WindowClosing
import de.sciss.pdflitz
import at.iem.point.illism.chart.HarmonicFields
import org.jfree.chart.{ChartPanel, StandardChartTheme, ChartFactory}

object Analysis extends SimpleSwingApplication {
  def xkorr = false
  def sum   = false

  lazy val top = new Frame {
    ChartFactory.setChartTheme(StandardChartTheme.createLegacyTheme())

    def mkChart(notes: Vec[OffsetNote], title: String): Component =
      if (xkorr) {
        HarmonicFields.contingencyChart(notes = notes, allIntervals = true, title = title)
      } else {
        val c = HarmonicFields.barChart(notes = notes, allIntervals = true, title = title)
        val p = new ChartPanel(c.peer, false)
        Component.wrap(p)
      }

    val charts = if (sum)
      mkChart(chordSeq.notes, "All intervals") :: Nil
    else
      chords.zipWithIndex.map { case (cs, i) =>
        val n = cs.flatMap(_.notes)
        mkChart(n, s"All intervals, sequence no. ${i+1} (${chordVoices(i)} voices)")
      }

    charts.foreach { c =>
      val d           = c.preferredSize
      d.width         = math.max(d.width, d.height)
      d.height        = d.width
      c.preferredSize = d
    }

    contents = new ScrollPane(new BoxPanel(Orientation.Vertical) {
      contents ++= charts
    })
    pack()
    centerOnScreen()
    open()

    reactions += {
      case WindowClosing(_) => quit()
    }

    new pdflitz.SaveAction(charts.map(c => c: pdflitz.Generate.Source)).setupMenu(this)
  }
}