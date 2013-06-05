package at.iem.point.sh.sketches
package genetic

import scala.collection.immutable.{IndexedSeq => Vec}
import at.iem.point.illism.rhythm.Ladma
import spire.math.Rational
import Fitness._

sealed trait Evaluation extends (Chromosome => Double)

case class GlobalEvaluation(fun: GlobalFunction, target: GlobalFunction, error: ErrorFunction)
  extends Evaluation {

  def apply(c: Chromosome): Double = {
    val eval  = fun(c)
    val t     = target(c)
    error(eval, t)
  }
}

case class WindowedEvaluation(window: WindowFunction, fun: LocalFunction, target: LocalFunction,
                              error: ErrorFunction, aggr: AggregateFunction)
  extends Evaluation {
  
  private val errorT = error.tupled.apply _

  def apply(c: Chromosome): Double = {
    val slices  = window(c)
    val evals   = slices.map(fun   )
    val targets = slices.map(target)
    val errors  = (evals zip targets).map(errorT)
    aggr(errors)
  }
}

object WindowFunction {
  // val all: Vec[WindowFunction] = ...

  case class Events(size: Int, step: Int) extends WindowFunction {
    def apply(c: Chromosome): Vec[Window] = {
      val slices    = slideByEvents(size, step)(c)
      val zipped    = flatWithAccum(c)
      val w1        = math.max(1, zipped.size - size)
      // require(w1 > 0, s"For $seq w1 is $w1")
      val w2        = w1.toDouble
      val m         = slices.map { case (off, idx, slice) =>
        val w       = math.min(1.0, idx.toDouble / w2)
        Window(slice, idx, off, w)
      }
      m
    }
  }
}
sealed trait WindowFunction extends (Chromosome => Vec[Window])

case class Window(sq: Sequence, idx: Int, offset: Rational, w: Double)

object GlobalFunction {
  case class Wrap(local: LocalFunction) extends GlobalFunction {
    def apply(sq: Chromosome): Double = {
      val win = Window(sq.flattenCells, 0, 0, 0.5)
      local(win)
    }
  }

  case class Const(d: Double) extends GlobalFunction {
    def apply(sq: Chromosome): Double = d
  }
}
sealed trait GlobalFunction extends (Chromosome       => Double)

object LocalFunction {
  val all: Vec[LocalFunction] = Vec(LadmaEntropy)

  case object LadmaEntropy extends LocalFunction {
    def apply(win: Window): Double = Ladma.entropy(win.sq.toCell)
  }

  case class Const(d: Double) extends LocalFunction {
    def apply(win: Window): Double = d
  }

  case class Line(lo: Double, hi: Double) extends LocalFunction {
    def apply(win: Window): Double = win.w.linlin(0, 1, lo, hi)
  }

  case class Exp(lo: Double, hi: Double) extends LocalFunction {
    def apply(win: Window): Double = win.w.linexp(0, 1, lo, hi)
  }

  case class ExpExp(lo: Double, hi: Double) extends LocalFunction {
    def apply(win: Window): Double = win.w.linexp(0, 1, lo, hi).linexp(lo, hi, lo, hi)
  }
}
sealed trait LocalFunction  extends (Window           => Double)

// sealed trait LocalTarget    extends (Window           => Double)

object ErrorFunction {
  val all: Vec[ErrorFunction] = Vec(Relative)

  case object Relative extends ErrorFunction {
    def apply(eval: Double, target: Double): Double = math.abs(eval - target) / target
  }
}
sealed trait ErrorFunction  extends ((Double, Double) => Double)

object AggregateFunction {
  val all: Vec[AggregateFunction] = Vec(Mean, RMS)

  case object Mean extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = errors.sum / errors.size
  }

  case object RMS extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = math.sqrt(errors.map(x => x * x).sum / errors.size)
  }
}
sealed trait AggregateFunction extends (Vec[Double] => Double)