/*
 *  SVM.scala
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

import de.sciss.file._
import java.io.PrintStream
import sys.process._
import de.sciss.{numbers, kollflitz}

object SVM extends App {
  def horiz       = false   // if `true` look at voice progression, if `false` only consider vertical structures
  def withBlind   = true    // if `true`, include formerly blind files

  def svmDir      = userHome / "Documents" / "devel" / "libsvm"
  def svmTrain    = svmDir / "svm-train"
  def svmPredict  = svmDir / "svm-predict"
  def svmScale    = svmDir / "svm-scale"
  def scale       = false   // doesn't improve, basically we're already scaled

  object SvmType {
    case object C_SVC    extends SvmType { def optionID = 0 }
    case object Nu_SVC   extends SvmType { def optionID = 1 }
    case object OneClass extends SvmType { def optionID = 2 }
    case object Eps_SVR  extends SvmType { def optionID = 3 }
    case object Nu_SVR   extends SvmType { def optionID = 4 }
  }
  sealed trait SvmType {
    def toOptions: List[String] = "-s" :: optionID.toString :: Nil
    def optionID: Int
  }

  object Kernel {
    case object Linear  extends Kernel { def optionID = 0 }
    case object Poly    extends Kernel { def optionID = 1 }
    case object Radial  extends Kernel { def optionID = 2 }
    case object Sigmoid extends Kernel { def optionID = 3 }
    case object Pre     extends Kernel { def optionID = 4 }
  }
  sealed trait Kernel {
    def toOptions: List[String] = "-t" :: optionID.toString :: Nil
    def optionID: Int
  }

  object TrainOptions {
    var tpe       : SvmType = SvmType.C_SVC   // default: C_SVC
    var kernel    : Kernel  = Kernel.Radial   // default: Radial
    var cCost     : Double  = 100 // 1.0             // default: 1      ; cost of constraints violation
    var shrinking : Boolean = true            // default: true
    var eps       : Double  = 0.001           // default: 0.001         ; tolerance of termination criterion

    def toOptions: List[String] = tpe.toOptions ++ kernel.toOptions ++ List("-c", cCost.toString) ++
      List("-h", (if (shrinking) 1 else 0).toString) ++ List("-e", eps.toString)
  }

  def kreuzVec(map: Map[Int, Map[Int, Int]]): Vec[Vec[Int]] =
    Vec.tabulate(7)(x => Vec.tabulate(x + 1)(y => map(x)(y)))

  def norm(vec: Vec[Int]): Vec[Double] = {
    val sum = vec.sum
    val f   = 1.0 / sum
    vec.map(_ * f)
  }

  def svmString(categ: Int, vec: Vec[Double]): String = {
    vec.zipWithIndex.map { case (num, fi) => s"${fi + 1}:$num" } .mkString(s"$categ ", " ", "")
  }

  def splitHalf[A](vec: Vec[A]): (Vec[A], Vec[A]) = {
    // vec.splitAt(vec.size/2)
    val (a, b) = vec.zipWithIndex.partition(_._2 % 2 == 0)   // abwechselnd in zwei teile
    a.map(_._1) -> b.map(_._1)
  }

  case class Feature(name: String, value: Double)

  type Features = Vec[Feature]

  case class Problem(label: Int, features: Features) {
    override def toString = svmString(label, features.map(_.value))
  }

  def processBlind(study: StudyLike): Features = {
    import Boring.Measure._
    import numbers.Implicits._
    import kollflitz.Ops._
    println(s"Processing '$study'...")
    lazy val an  = Kreuztabelle.analyze(study, allIntervals = true, intervalClasses = true, horiz = horiz,
      chordSize = if (horiz) 4 else -1)
    lazy val x   = kreuzVec(an)
    lazy val no  = norm(x.flatten)
    lazy val (noM, noV) = no.meanVariance
    lazy val noS = noV.sqrt

    lazy val kreuz = {
      val f   = x.flatten
      val sum = f.sum
      f.map(_.toDouble / sum)
    }

    lazy val ha  = Boring.process(study, measure = HorizAmbi)
    lazy val (haM, haV) = ha.meanVariance
    lazy val haS = haV.sqrt

    lazy val hv  = Boring.process(study, measure = HorizVar)
    lazy val (hvM, hvV) = hv.meanVariance
    lazy val hvS = hvV.sqrt

    lazy val cv  = Boring.process(study, measure = ChordVar)
    lazy val (cvM, cvV) = cv.meanVariance
    lazy val cvS = cvV.sqrt

    lazy val haM_mix_hvM = hvM / haM

    // val features = no :+ mean :+ std
    // val features = Vec(haM, haS, hvM, hvS, cvM, cvS)
    // val features = no ++ Vec(haM, hvM, cvM)

    // val features = Vec(noS, haM, hvM, cvM)

    lazy val feat1 = Vec(
      Feature("noM", noM),
      Feature("noV", noV),
      Feature("noS", noS),
      Feature("haM", haM),
      Feature("haV", haV),
      Feature("haS", haS),
      Feature("hvM", hvM),
      Feature("hvV", hvV),
      Feature("hvS", hvS),
      Feature("cvM", cvM),
      Feature("cvV", cvV),
      Feature("cvS", cvS)
    )

    lazy val feat2 = kreuz.zipWithIndex.map { case (v, i) => Feature(s"x$i", v) }

    lazy val feat3 = {
      val x = OtherFeatures.horizPitchClasses(study)
      x.zipWithIndex.map { case (v, i) => Feature(s"x$i", v) }
    }

    // weighted best
    lazy val featCombi1 = {
      val vSel = Vector(3, 14, 18, 19, 20, 21) // for vertical
      val hSel = Vector(10, 13, 14, 15, 23, 44) // for horizontal
      vSel.map(feat2(_)) ++ hSel.map(feat3(_))
    }

    // absolute best // THIS ONE
    lazy val featCombi2 = {
      val vSel = Vector(2, 14, 16, 18, 19, 21) // for vertical
      val hSel = Vector(10, 13, 15, 33, 47) // for horizontal
      vSel.map(feat2(_)) ++ hSel.map(feat3(_))
    }

    // absolute second / third best
    lazy val featCombi3 = {
      val vSel = Vector(3, 14, 22, 26, 27) // for vertical
      val hSel = Vector(10, 13, 14, 40, 44) // for horizontal
      vSel.map(feat2(_)) ++ hSel.map(feat3(_))
    }

    // val res = svmString(boring = study.isBoring, vec = features)
    // featCombi2 /* feat3 */ /* feat1 */ /* feat2 */

    feat2
  }

  def process(study: Study): Problem = {
    val f = processBlind(study)
    Problem(label = if (study.isBoring) 0 else 1, features = f)
  }

  // javax.sound.midi.MidiSystem.getMidiFileTypes

  def newBoring = Seq(101, 102, 103, 104, 106, 107, 108, 112, 116, 117,
    "134A", "134B", "134C", "134D", "134E", "134H", "134I", "134K",
    "134L", "134M", "134N", "134O", "134P", "134Q", "134S", "134T")
    .map { idx0 =>
      val idx = idx0 match {
        case i: Int => i
        case _      => 134
      }
      val file = recPath / "blind" / s"Study_#${idx0}.mid"
      Study(idx = idx, isBoring = true, file = file)
    }

  def newPromising = Seq(111, 113,
    "134F", "134G", "134J", "134L", "134R", "134U")
    .map { idx0 =>
      val idx = idx0 match {
        case i: Int => i
        case _      => 134
      }
      val file = recPath / "blind" / s"Study_#${idx0}.mid"
      Study(idx = idx, isBoring = false, file = file)
    }

  //  def allBoringProblems     = allBoring   .map(process) ++ (if (withBlind) newBoring   .map(process) else Nil)
  //  def allPromisingProblems  = allPromising.map(process) ++ (if (withBlind) newPromising.map(process) else Nil)

  def allBoringProblems     = newBoring   .map(process) .toIndexedSeq
  def allPromisingProblems  = newPromising.map(process) .toIndexedSeq
  def allProblems           = allBoringProblems ++ allPromisingProblems

  def allBlindFeatures      = allBlind    .map(processBlind)

  def run(): Unit = {
    val (bTrain, bTest) = splitHalf(allBoringProblems   )
    val (pTrain, pTest) = splitHalf(allPromisingProblems)

    val allTrain = bTrain ++ pTrain
    val allTest  = bTest  ++ pTest

    val fTrain    = tempFile("train")
    val fModel    = tempFile("model")
    val fTest     = tempFile("test")
    val fPredict  = tempFile("predict")

    svmWrite(fTrain, allTrain)
    svmWrite(fTest , allTest )

    println(s"\n\nRunning SVM Analysis... dataPath = ${dataPath(fTrain)}\n")

    val cmdTrain  = Seq(svmTrain.path) ++ TrainOptions.toOptions ++ Seq(dataPath(fTrain), fModel.path)
    println(cmdTrain.mkString(" "))
    val resTrain  = cmdTrain.!
    require(resTrain == 0, s"svm-train failed with code $resTrain")

    val cmdPredict = Seq(svmPredict.path, dataPath(fTest), fModel.path, fPredict.path)
    println(cmdPredict.mkString(" "))
    val resPredict = cmdPredict.!
    require(resPredict == 0, s"svm-predict failed with code $resTrain")
  }

  def tempFile(name: String): File = {
    val dir = file("analysis") / "svm"
    dir.mkdirs()
    dir / name
  }

  def svmWrite(file: File, lines: Vec[Problem]): Unit = {
    val f1  = if (scale) File.createTemp() else file
    val out = new PrintStream(f1, "UTF-8")
    lines.foreach(out.println)
    out.close()
    if (scale) {
      val scaleCmd  = Seq(svmScale.path, f1.path)
      println(scaleCmd.mkString(" "))
      val scaled    = scaleCmd.!!
      // println(s"---scaled---:\n$scaled")
      val out1      = new PrintStream(file, "UTF-8")
      out1.print(scaled)
      out1.close()
    }
  }

  def dataPath(f: File): String = f.path
    // (if (scale) f.parent / s"${f.base}.scale" else f).path

  def normalize(in: Vec[Problem]): Vec[Problem] = {
    val newFeat = normalizeFeatures(in.map(_.features))
    val res = (in zip newFeat).map { case (pOld, fNew) =>
      pOld.copy(features = fNew)
    }
    res
  }

  def normalizeFeatures(in: Vec[Features]): Vec[Features] = {
    import de.sciss.kollflitz.Ops._
    import de.sciss.numbers.Implicits._

    val t = in.map(_.map(_.value)).transpose
    val n = t.map { col =>
      val (mn, mx) = (col.min, col.max)
      col.map(_.linlin(mn, mx, 0.0, 1.0))
    }
    (in zip n.transpose).map { case (fOlds, vecNew) =>
      (fOlds zip vecNew).map {
        case (fOld, valNew) =>
          fOld.copy(value = valNew)
      }
    }
  }
}