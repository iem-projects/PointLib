package at.iem.point.ms.sketches

import de.sciss.file._
import java.io.PrintStream

object SVM extends App {
  def svmDir      = userHome / "Documents" / "devel" / "libsvm"
  def svmTrain    = svmDir / "svm-train"
  def svmPredict  = svmDir / "svm-predict"

  def kreuzVec(map: Map[Int, Map[Int, Int]]): Vec[Vec[Int]] =
    Vec.tabulate(7)(x => Vec.tabulate(x + 1)(y => map(x)(y)))

  def norm(vec: Vec[Int]): Vec[Double] = {
    val sum = vec.sum
    val f   = 1.0 / sum
    vec.map(_ * f)
  }

  def svmString(boring: Boolean, vec: Vec[Double]): String = {
    val categ = if (boring) "0" else "1"
    vec.zipWithIndex.map { case (num, fi) => s"${fi + 1}:$num" } .mkString(s"$categ ", " ", "")
  }

  def splitHalf[A](vec: Vec[A]): (Vec[A], Vec[A]) = vec.splitAt(vec.size/2)

  def process(study: Study): String = {
    println(s"Processing '$study'...")
    val an  = Kreuztabelle.analyze(study, allIntervals = true, intervalClasses = true)
    val no  = norm(kreuzVec(an).flatten)
    svmString(boring = study.isBoring, vec = no)
  }

  val (bTrain, bTest) = splitHalf(allBoring   .map(process))
  val (pTrain, pTest) = splitHalf(allPromising.map(process))

  val allTrain = bTrain ++ pTrain
  val allTest  = bTest  ++ bTest

  def tempFile(name: String): File = {
    val dir = file("analysis") / "svm"
    dir.mkdirs()
    dir / name
  }

  val fTrain    = tempFile("train")
  val fModel    = tempFile("model")
  val fTest     = tempFile("test")
  val fPredict  = tempFile("predict")
  
  def svmWrite(file: File, lines: Vec[String]): Unit = {
    val out = new PrintStream(file, "UTF-8")
    lines.foreach(out.println)
    out.close()
  }

  import sys.process._

  svmWrite(fTrain, allTrain)
  svmWrite(fTest , allTest )

  println("\n\nRunning SVM Analysis\n")
  
  val resTrain = Seq(svmTrain.path, fTrain.path, fModel.path).!
  require(resTrain == 0, s"svm-train failed with code $resTrain")

  val resPredict = Seq(svmPredict.path, fTest.path, fModel.path, fPredict.path).!
  require(resPredict == 0, s"svm-predict failed with code $resTrain")

  // File.revealInFinder(svmIn)
}
