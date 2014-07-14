/*
 *  BlindPrediction.scala
 *  (PointLib - ms)
 *
 *  Copyright (c) 2013-2014 IEM Graz / Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package at.iem.point.ms.sketches

import scala.collection.breakOut
import java.awt.EventQueue
import de.sciss.file._

object BlindPrediction extends App {
  val problems: Vec[SVM.Problem ] = SVM.normalize(SVM.allProblems)
  // val blindInput = allBoring ++ allPromising
  val blindInput = allBlind
  val blindFeat: Vec[SVM.Features] = SVM.normalizeFeatures(blindInput.map(SVM.processBlind))
  // val blind   : Vec[SVM.Features] = SVM.normalizeFeatures(SVM.allBlindFeatures)

  require(problems.size == 46)
  val numFeatures: Int = problems.head.features.size
  require(numFeatures == 11)
  val featureNames: Vec[String] = problems.head.features.map(_.name)
  val labels: Vec[(Int, Int)] = problems.groupBy(_.label).mapValues(_.size).toIndexedSeq.sortBy(_._1)
  val histo = labels.toMap

  lazy val paramText = {
    val ws  = weights.map { case (label, c) => f"-w $label $c%1.2f" } .mkString(" ")
    s"-t 2 -c 100 $ws -g 1"
  }

  lazy val applet: SVMVis = {
    val res = new SVMVis()
    res.paramText = paramText
    println(s"\nParameters: $paramText")
    res
  }

  lazy val weights = genWeights(problems)

  def genWeights(p: Vec[SVM.Problem]): Vec[(Int, Double)] = {
    // val freq  = p.groupBy(_.label).mapValues(_.size) // .toIndexedSeq.sortWith(_._1)
    val sz    = p.size
    val c0s   = histo.mapValues(num => (sz - num).toDouble / num)
    val min   = c0s.valuesIterator.min
    c0s.map { case (label, c0) => label -> c0 / min } (breakOut)
  }

  def charlie(): Unit = {
    val tr      = problems // .dropRight(1)
    val ts      = blindFeat // problems // .drop(1)

    val param   = applet.parameters(numFeatures)
    val trP     = applet.mkProblem(tr)(_.label)(p => p.features.map(_.value))
    val model   = applet.train(trP, param)

    val tsP     = applet.mkProblem(ts)(_ => 0)(p => p.map(_.value))
    val pred    = applet.predict(model, tsP)

    println()
    (blindInput zip pred).foreach { case (study, categ) =>
      val promising = categ == 1
      println(s"${study.file.name} - ${if (promising) "promising" else "boring"}")
    }

    //    val sz = ts.size
    //    var mxl = 0
    //    var i = 0; while (i < sz) {
    //      val tl = ts(i).label
    //      if (tl >= mxl) mxl = tl + 1
    //      i += 1
    //    }
    //    val tot  = new Array[Int](mxl)
    //    val corr = new Array[Int](mxl)
    //    i = 0; while (i < sz) {
    //      val tl = ts(i).label
    //      tot(tl) += 1
    //      if (pred(i) == tl) corr(tl) += 1
    //      i += 1
    //    }
    //    val mb = Map.newBuilder[Int, (Int, Int)]
    //    i = 0; while (i < mxl) {
    //      val t = tot(i)
    //      if (t > 0) mb += (i -> (t, corr(i)))
    //      i += 1
    //    }
    //    val m2 = mb.result()

    //    val (tot, abs) = m.valuesIterator.reduce[(Int, Int)] { case ((tot1, corr1), (tot2, corr2)) =>
    //      (tot1 + tot2) -> (corr1 + corr2)
    //    }
    // m2

    //    val res = ((0, 1.0) /: m2.valuesIterator) { case ((abs, rel), (tot1, corr1)) =>
    //      val rel1 = corr1.toDouble / tot1
    //      (abs + corr1 /* tot */, math.min(rel, rel1))
    //    }
    //
    //    println(s"Result: $res")
  }

  EventQueue.invokeLater(new Runnable {
    def run(): Unit = {
      charlie()
      println("\nDone.")
    }
  })
}