package at.iem.point.ms.sketches

import de.sciss.file._
import java.io.PrintStream
import sys.process._

object SVM extends App {
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
    var tpe   : SvmType = SvmType.C_SVC   // default: C_SVC
    var kernel: Kernel  = Kernel.Radial   // default: Radial
    var cCost : Double  = 1.0             // default: 1
    var shrinking: Boolean = true         // default: true

    def toOptions: List[String] = tpe.toOptions ++ kernel.toOptions ++ List("-c", cCost.toString) ++
      List("-h", (if (shrinking) 1 else 0).toString)
  }

  def kreuzVec(map: Map[Int, Map[Int, Int]]): Vec[Vec[Int]] =
    Vec.tabulate(7)(x => Vec.tabulate(x + 1)(y => map(x)(y)))

  def norm(vec: Vec[Int]): Vec[Double] = {
    val sum = vec.sum
    val f   = 1.0 / sum
    vec.map(_ * f)
  }

  def svmString(boring: Boolean, vec: Vec[Double]): String = {
    val categ = if (boring) 0 else 1
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
  val allTest  = bTest  ++ pTest

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
    val f1  = if (scale) File.createTemp() else file
    val out = new PrintStream(f1, "UTF-8")
    lines.foreach(out.println)
    out.close()
    if (scale) {
      val scaled  = Seq(svmScale.path, file.path).!!
      val out1    = new PrintStream(file, "UTF-8")
      out1.print(scaled)
      out1.close()
    }
  }

  def dataPath(f: File): String = f.path
    // (if (scale) f.parent / s"${f.base}.scale" else f).path

  svmWrite(fTrain, allTrain)
  svmWrite(fTest , allTest )

  println("\n\nRunning SVM Analysis\n")
  
  val resTrain = (Seq(svmTrain.path) ++ TrainOptions.toOptions ++ Seq(dataPath(fTrain), fModel.path)).!
  require(resTrain == 0, s"svm-train failed with code $resTrain")

  val resPredict = Seq(svmPredict.path, dataPath(fTest), fModel.path, fPredict.path).!
  require(resPredict == 0, s"svm-predict failed with code $resTrain")

  // File.revealInFinder(svmIn)
}
