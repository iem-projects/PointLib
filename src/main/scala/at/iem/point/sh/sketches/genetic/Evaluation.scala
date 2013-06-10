package at.iem.point.sh.sketches
package genetic

import scala.collection.immutable.{IndexedSeq => Vec}
import at.iem.point.illism.rhythm.Ladma
import spire.math.Rational
import Fitness._
import language.existentials

object Evaluation {
  case class Windowed(window: WindowFunction    = WindowFunction.Events(),
                      fun   : LocalFunction     = LocalFunction.LadmaEntropy,
                      target: LocalFunction     = LocalFunction.Const(0.2),
                      error : ErrorFunction     = ErrorFunction.Relative,
                      aggr  : AggregateFunction = AggregateFunction.Mean)
    extends Evaluation {

    private val errorT = error.tupled.apply _

    def apply(c: Chromosome): Double = {
      val slices  = window(c)
      val evals   = slices.map(fun   )
      val targets = slices.map(target)
      val errors  = (evals zip targets).map(errorT)
      aggr(errors)
    }

    // def meta = Meta[WindowedEvaluation]
  }

  case class Global(fun   : GlobalFunction = GlobalFunction.Const(),
                    target: GlobalFunction = GlobalFunction.Const(),
                    error : ErrorFunction  = ErrorFunction.Relative)
    extends Evaluation {

    def apply(c: Chromosome): Double = {
      val eval  = fun(c)
      val t     = target(c)
      error(eval, t)
    }

    // def meta = Meta[GlobalEvaluation]
  }
}
sealed trait Evaluation extends (Chromosome => Double) /* with HasMeta[Evaluation] */

object WindowFunction {
  // val all: Vec[Meta[WindowFunction]] = Vec(Meta[Events])

  case class Events(size: Int = 5, step: Int = 2) extends WindowFunction {
    require(step >= 1 && size >= step)

    def apply(c: Chromosome): Vec[Slice] = {
      val slices    = slideByEvents(size, step)(c)
      val zipped    = flatWithAccum(c)
      val w1        = math.max(1, zipped.size - size)
      // require(w1 > 0, s"For $seq w1 is $w1")
      val w2        = w1.toDouble
      val m         = slices.map { case (off, idx, slice) =>
        val w       = math.min(1.0, idx.toDouble / w2)
        Slice(slice, idx, off, w)
      }
      m
    }

    // def meta = Meta[Events]
  }
}
sealed trait WindowFunction extends (Chromosome => Vec[Slice]) /* with HasMeta[WindowFunction] */

case class Slice(sq: Sequence, idx: Int, offset: Rational, w: Double)

object GlobalFunction {
  // val all: Vec[Meta[GlobalFunction]] = Vec(Meta[Const])

  case class Wrap(local: LocalFunction) extends GlobalFunction {
    def apply(sq: Chromosome): Double = {
      val win = Slice(sq.flattenCells, 0, 0, 0.5)
      local(win)
    }

    // def meta = Meta[Wrap]
  }

  case class Const(d: Double = 0.0) extends GlobalFunction {
    def apply(sq: Chromosome): Double = d

    // def meta = Meta[Const]
  }
}
sealed trait GlobalFunction extends (Chromosome => Double) /* with HasMeta[GlobalFunction] */

object LocalFunction {
  // val all: Vec[Meta[LocalFunction]] = Vec(Meta[LadmaEntropy.type], Meta[Const], Meta[Line], Meta[Exp], Meta[ExpExp])

  case object LadmaEntropy extends LocalFunction {
    def apply(win: Slice): Double = Ladma.entropy(win.sq.toCell)
    // def meta = Meta[LadmaEntropy.type]
  }

  case object Velocity extends LocalFunction {
    def apply(win: Slice): Double = {
      val seq1    = win.sq.bindTrailingRests

      // the geometric average is n-th root of the product of the durations
      val prod    = seq1.product
      val e       = math.pow(prod.toDouble, 1.0/seq1.size)
      e
    }
  }

  case class Const(d: Double = 0.0) extends LocalFunction {
    def apply(win: Slice): Double = d
    // def meta = Meta[Const]
  }

  case class Line(lo: Double = 0.0, hi: Double = 1.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linlin(0, 1, lo, hi)
    // def meta = Meta[Line]
  }

  case class Exp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi)
    // def meta = Meta[Exp]
  }

  case class ExpExp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi).linexp(lo, hi, lo, hi)
    // def meta = Meta[ExpExp]
  }
}
sealed trait LocalFunction extends (Slice => Double) /* with HasMeta[LocalFunction] */

// sealed trait LocalTarget    extends (Slice           => Double)

object ErrorFunction {
  // val all: Vec[Meta[ErrorFunction]] = Vec(Meta[Relative.type])

  case object Relative extends ErrorFunction {
    def apply(eval: Double, target: Double): Double = math.abs(eval - target) / target

    // def meta = Meta[Relative.type]
  }
}
sealed trait ErrorFunction  extends ((Double, Double) => Double) /* with HasMeta[ErrorFunction] */

object AggregateFunction {
  // val all: Vec[Meta[AggregateFunction]] = Vec(Meta[Mean.type], Meta[RMS.type])

  case object Mean extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = errors.sum / errors.size

    // def meta = Meta[Mean.type]
  }

  case object RMS extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = math.sqrt(errors.map(x => x * x).sum / errors.size)

    // def meta = Meta[RMS.type]
  }
}
sealed trait AggregateFunction extends (Vec[Double] => Double) /* with HasMeta[AggregateFunction] */