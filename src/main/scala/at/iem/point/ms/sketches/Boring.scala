package at.iem.point.ms.sketches

import scala.swing.Swing
import GUI._
import Swing._
import at.iem.point.illism._
import at.iem.point.illism.rhythm.{Ladma, numericCompat}
import spire.math.Rational

object Boring extends App {
  Swing.onEDT {
    val which = args.headOption.getOrElse("?")

    which match {
      case "--kreuz"  => kreuz()
      case _          => ladmaVerlauf()
    }
  }

  def kreuz() {
    val panes = Seq(true, false).flatMap { all =>
      val c1 = Kreuztabelle.analyze(Study.Boring(26), allIntervals = all)
      val c2 = Kreuztabelle.analyze(Study.Raw   ( 5), allIntervals = all)
      Seq(c1, c2)
    }
    val panel = panes.asGrid(2, 2)
    frame("Kreuztabelle", panel, (1000, 1000))
  }

  def ladmaVerlauf() {
    val measure = "mobility"

    // val sts = Vector(Study.Boring(26), Study.Raw(5))
    // val sts = Vector(Study.Boring(29), Study.Raw(4), Study.Raw(5), Study.Raw(6) /*, Study.Raw(7) */)

    // val sts     = Vector(Study.Boring(26)) // , Study.Boring(29)) // , Study.Boring(31))
    // val sts     = Vector(Study.Raw(4), Study.Raw(5), Study.Raw(6)) // , Study.Raw(7))
    val sts     = Vector(Study.Boring(29), Study.Raw(4), Study.Raw(5), Study.Raw(6))
    val winSecs = 16.0
    val winOver = 8

    val ms  = sts.map { st =>
      println(s"\nFile: ${st.file.name}")
      val notes     = load(st).notes
      val tup = {
        // the problem with findHarmonicFields is that they overlap
        // val segm      = ChordUtil.findHarmonicFields(notes, minPoly = 1)
        // segm.map(c => c.avgOffset -> c.avgDuration)
        val tFlt  = NoteUtil.clean(notes, minDuration = 0.1)
        val stabs = NoteUtil.stabbings(tFlt)
        val _durs = stabs.pairDiff
        stabs zip _durs
      }
      val (segm, durRaw) = tup.unzip
      // val quant     = Tempo.guess(durRaw, maxDenom = if (st.isInstanceOf[Study.Boring]) 128 else 128)
      val quant = Tempo.quantize(durRaw, qpm = 160, maxDenom = 64)
      println(s"Tempo: ${quant.tempoBase} = ${quant.tempoNom}")

      import NoteUtil2.noteOffsetView
      val winSize = winSecs / quant.wholeDuration  // wholes per 10 seconds
      val winStep = winSize / winOver
      println(f"Sliding window size $winSize, step $winStep (1 = ${quant.wholeDuration}%1.3fs)")

      assert(segm.size == quant.durations.size)
      val segmQuant = segm zip quant.durations
      val slices0   = NoteUtil2.slidingWindow(segmQuant, size = winSize, step = winStep)(_._1)
      val slices    = slices0.dropRight(winOver*2/3) // last slice is smaller and produces spikes

      val cells     = slices.zipWithIndex.flatMap { case (xs, i) =>
        val dursQuant = xs.map(_._2)
        val dur = dursQuant.sum
        if (dur == 0) None else Some(
          rhythm.Cell(i, dursQuant.map(rhythm.Note(_)), dur)
        )
      }
      val mf  = measure match {
        case "entropy"  => Ladma.entropy  _
        case "mobility" => Ladma.mobility _
        case "tension"  => Ladma.tension  _
      }
      val m = cells.map(mf)
      m
    }

    import Plotting._

    ms.plot(ylabel = measure.capitalize, title = s"${measure.capitalize} Comparison", legends = sts.map(_.file.name))
  }
}