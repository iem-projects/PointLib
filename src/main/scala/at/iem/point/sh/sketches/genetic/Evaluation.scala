package at.iem.point.sh.sketches
package genetic

import scala.collection.immutable.{IndexedSeq => Vec}
import at.iem.point.illism.rhythm.{Note, Cell, Ladma}
import spire.math.Rational
import Fitness._
import language.existentials

object Evaluation {
  case class Windowed(window: WindowFunction    = WindowFunction.Events(),
                      fun   : LocalFunction     = LocalFunction.Velocity,
                      target: LocalFunction     = LocalFunction.Exp(0.0625, 0.25),
                      fit   : MatchFunction     = MatchFunction.RelativeNegative,
                      aggr  : AggregateFunction = AggregateFunction.Mean)
    extends Evaluation {

    private val fitT = fit.tupled.apply _

    def apply(c: Chromosome): Double = {
      val slices  = window(c)
        val evals   = slices.map(fun   )
      val targets = slices.map(target)
      val errors  = (evals zip targets).map(fitT)
      aggr(errors)
    }
  }

  case class Global(fun   : Evaluation    = Evaluation.Const(),
                    target: Evaluation    = Evaluation.Const(),
                    error : MatchFunction = MatchFunction.RelativeReciprocal)
    extends Evaluation {

    def apply(c: Chromosome): Double = {
      val eval  = fun(c.map(_.normalized))
      val t     = target(c)
      error(eval, t)
    }
  }

  case class Wrap(local: LocalFunction) extends Evaluation {
    def apply(sq: Chromosome): Double = {
      val win = Slice(sq.flattenCells, 0, 0, 0.5)
      local(win)
    }
  }

  case class Const(d: Double = 0.0) extends Evaluation {
    def apply(sq: Chromosome): Double = d
  }

  /** Counts the number of duplicate cells in a chromosome. */
  case class DuplicateCells(ignoreStretch: Boolean = true) extends Evaluation {
    def apply(sq: Chromosome): Double = {
      val grouped = if (ignoreStretch) {
        sq.groupBy(_.id)
      } else {
        sq.groupBy(c => (c.id, c.dur))
      }
      val numUnique = grouped.size
      val numDup    = sq.size - numUnique
      numDup
    }
  }

  /** Produces a sequential evaluation, by passing the chromosome first through a `Chromosome => Chromosome`
    * function and then through a subsequent `Chromosome => Double` evaluation.
    */
  case class Serial(a: ChromosomeFunction = ChromosomeFunction.BindTrailingRests,
                    b: Evaluation = Windowed())  extends Evaluation {
    override def apply(c: Chromosome): Double = b(a(c))
  }

  case class Parallel(a: Evaluation     = Windowed(),
                      b: Evaluation     = Global(DuplicateCells(), Const(1), MatchFunction.LessThan),
                      op: MatchFunction = MatchFunction.Times) extends Evaluation {
    override def apply(c: Chromosome): Double = op(a(c), b(c))
  }
}

/** An `Evaluation` is the container for the whole fitness evaluation process, taking an input chromosome
  * and returning a single fitness value.
  */
sealed trait Evaluation extends (Chromosome => Double)

object ChromosomeFunction {
  case object BindTrailingRests extends ChromosomeFunction {
    override def apply(c: Chromosome): Chromosome = {
      val durs  = c.flattenCells.bindTrailingRests
      val cell  = durs.map(Note(_)).toCell
      Vec(cell)
    }
  }
}
/** A `ChromosomeFunction` pre-processes a chromosome before it is sent to another evaluation. */
sealed trait ChromosomeFunction extends (Chromosome => Chromosome)

object WindowFunction {
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
  }
}
/** A `WindowFunction` produces a sliding window view of a chromosome, by flattening its cells and returning a
  *  sequence of cell sequences. */
sealed trait WindowFunction extends (Chromosome => Vec[Slice])

/** A slice is a cell sequence which was taken from a chromosome.
  *
  * @param sq       the cell sequence
  * @param idx      the event offset index in the original chromosome
  * @param offset   the time offset in the original chromosome
  * @param w        the weight from 0 to 1 for this logical window across the original chromosome
  */
case class Slice(sq: Sequence, idx: Int, offset: Rational, w: Double) {
  def size: Int = sq.size
}

object LocalFunction {
  case object LadmaEntropy extends LocalFunction {
    def apply(win: Slice): Double = Ladma.entropy(win.sq.toCell)
  }

  case object Velocity extends LocalFunction {
    def apply(win: Slice): Double = {
      val seq1    = if (win.size > 1) win.sq.bindTrailingRests else win.sq.map(_.dur)

      // the geometric average is n-th root of the product of the durations
      val prod    = seq1.product
      val e       = math.pow(prod.toDouble, 1.0/seq1.size)
      e
    }
  }

  case class Const(d: Double = 0.0) extends LocalFunction {
    def apply(win: Slice): Double = d
  }

  case class Line(lo: Double = 0.0, hi: Double = 1.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linlin(0, 1, lo, hi)
  }

  case class Exp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi)
  }

  case class ExpExp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi).linexp(lo, hi, lo, hi)
  }
}
sealed trait LocalFunction extends (Slice => Double)

object MatchFunction {
  /** The relative match, which is the reciprocal of the relative error. This is limited to 1 per mille
    * relative error, in order not to produce infinitely good matches, which would be bad for aggregation.
    */
  case object RelativeReciprocal extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      if (eval == target) 1000 else math.min(1000, target / math.abs(eval - target))
    }
  }

  case object RelativeNegative extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      1000 - (if (eval == target) 0 else math.min(1000, math.abs(eval - target) / target))
    }
  }

  case object AbsoluteReciprocal extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      math.min(1000, 1.0 / math.abs(eval - target))
    }
  }

  case object Min extends MatchFunction {
    def apply(a: Double, b: Double): Double = math.min(a, b)
  }

  case object Times extends MatchFunction {
    def apply(a: Double, b: Double): Double = a * b
  }

  case object LessThan extends MatchFunction {
    def apply(a: Double, b: Double): Double = if (a < b) 1 else 0
  }
}
sealed trait MatchFunction  extends ((Double, Double) => Double)

object AggregateFunction {
  case object Mean extends AggregateFunction {
    def apply(fits: Vec[Double]): Double = fits.sum / fits.size
  }

  case object RMS extends AggregateFunction {
    def apply(fits: Vec[Double]): Double = 1.0 / math.sqrt(fits.map(x => 1.0/(x * x)).sum / fits.size)
  }

  case object Min extends AggregateFunction {
    def apply(fits: Vec[Double]): Double = fits.min
  }
}
sealed trait AggregateFunction extends (Vec[Double] => Double)