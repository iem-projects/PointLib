package at.iem.point.er.sketches

import java.text.{ParsePosition, FieldPosition, NumberFormat}

import org.jfree.chart.axis.NumberAxis

import scala.swing.Swing
import scalax.chart.api._

object PDFLogAxis extends App {
  Swing.onEDT(run())

  def midicps(d: Double): Double = 440 * math.pow(2, (d - 69) / 12)
  def cpsmidi(d: Double): Double = math.log(d / 440) / math.log(2) * 12 + 69

  def run(): Unit = {
    val lo    = cpsmidi(32.7)
    val hi    = cpsmidi(16.7e3)
    val data  = Vector((0.0, lo), (1.0, hi))
    val chart = XYLineChart(data, title = "", legend = false)
    val yAxis = chart.plot.range.axis.peer.asInstanceOf[NumberAxis]
    yAxis.setLowerBound(lo)
    yAxis.setUpperBound(hi)
    yAxis.setNumberFormatOverride(new NumberFormat {
      def format(d: Double, sb: StringBuffer, pos: FieldPosition): StringBuffer = {
        val freq = midicps(d)
        sb.append(f"$freq%1.1f")
      }

      def parse(source: String, parsePosition: ParsePosition): Number = ???

      def format(number: Long, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer = ???
    })
    // chart.show()

    import ChartSupport._
    chart.printableLook()
    showChart(chart, 100, 300)
  }
}
