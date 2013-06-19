package at.iem.point.sh.sketches
package genetic

import scala.collection.immutable.{IndexedSeq => Vec}
import at.iem.point.illism.rhythm.{NoteOrRest, Note, Ladma}
import collection.breakOut
import spire.math.Rational
import Fitness._
import language.existentials

// object Evaluation {
  case class EvalWindowed(window: WindowFunction    = /* WindowFunction. */ WindowEvents(),
                          fun   : LocalFunction     = /* LocalFunction. */ GeomMean,
                          target: LocalFunction     = /* LocalFunction. */ Exp(0.0625, 0.25),
                          fit   : MatchFunction     = /* MatchFunction. */ MatchRelNegative,
                          aggr  : AggregateFunction = /* AggregateFunction. */ AggrMean)
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

  case class EvalGlobal(fun   : Evaluation    = /* Evaluation. */ EvalConst(),
                        target: Evaluation    = /* Evaluation. */ EvalConst(),
                        error : MatchFunction = /* MatchFunction. */ MatchRelReciprocal)
    extends Evaluation {

    def apply(c: Chromosome): Double = {
      val eval  = fun(c.map(_.normalized))
      val t     = target(c)
      error(eval, t)
    }
  }

  case class EvalWrap(local: LocalFunction) extends Evaluation {
    def apply(sq: Chromosome): Double = {
      val win = Slice(sq.flattenCells, 0, 0, 0.5)
      local(win)
    }
  }

case class EvalEvenOdd(even: Evaluation, odd: Evaluation, combine: MatchFunction) extends Evaluation {
  override def apply(c: Chromosome): Double = {
    val sq          = c.flattenCells
    val (ite, ito)  = sq.view.zipWithIndex.partition(_._2 % 2 == 0)
    val sqe         = ite.map(_._1).toIndexedSeq.toCell
    val sqo         = ito.map(_._1).toIndexedSeq.toCell
    val rese        = even(Vec(sqe))
    val reso        = odd (Vec(sqo))
    combine(rese, reso)
  }
}

  case class EvalConst(d: Double = 0.0) extends Evaluation {
    def apply(sq: Chromosome): Double = d
  }

  /** Counts the number of duplicate cells in a chromosome. */
  case class EvalDuplicateCells(ignoreStretch: Boolean = true) extends Evaluation {
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
  case class EvalSerial(a: ChromosomeFunction = /* ChromosomeFunction. */ BindTrailingRests,
                        b: Evaluation = EvalWindowed())  extends Evaluation {
    override def apply(c: Chromosome): Double = b(a(c))
  }

  case class EvalParallel(a: Evaluation     = EvalWindowed(),
                          b: Evaluation     = EvalGlobal(EvalDuplicateCells(), EvalConst(1), /* MatchFunction. */ MatchLessThan),
                          op: MatchFunction = /* MatchFunction. */ MatchTimes) extends Evaluation {
    override def apply(c: Chromosome): Double = op(a(c), b(c))
  }
// }

/** An `Evaluation` is the container for the whole fitness evaluation process, taking an input chromosome
  * and returning a single fitness value.
  */
sealed trait Evaluation extends (Chromosome => Double)

// object ChromosomeFunction {
  case object BindTrailingRests extends ChromosomeFunction {
    override def apply(c: Chromosome): Chromosome = {
      val durs  = c.flattenCells.bindTrailingRests
      val cell  = durs.map(Note(_)).toCell
      Vec(cell)
    }
  }
// }
/** A `ChromosomeFunction` pre-processes a chromosome before it is sent to another evaluation. */
sealed trait ChromosomeFunction extends (Chromosome => Chromosome)

// object WindowFunction {
  case class WindowEvents(size: Int = 5, step: Int = 2) extends WindowFunction {
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
// }
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

// object LocalFunction {
  case object LadmaEntropy extends LocalFunction {
    def apply(win: Slice): Double = Ladma.entropy(win.sq.toCell)
  }

  /** Geometric mean of the durations within a slice */
  case object GeomMean extends LocalFunction {
    def apply(win: Slice): Double = {
      if (win.size == 1) return win.sq.head.dur.toDouble
      val seq1    = win.sq.map(_.dur) // win.sq.bindTrailingRests // note: we can do this through serial function now
      // the geometric average is n-th root of the product of the durations
      val prod    = seq1.product
      val e       = math.pow(prod.toDouble, 1.0/seq1.size)
      e
    }
  }

case object NumPauses extends LocalFunction {
  def apply(win: Slice): Double = win.sq.count(_.isRest)
}

  case object StdDev extends LocalFunction {
    def apply(win: Slice): Double = {
      if (win.size == 1) return 0.0
      val seq1    = win.sq.map(_.dur)
      // the geometric average is n-th root of the product of the durations
      val mean    = (seq1.sum / seq1.size).toDouble
      val sumSqr  = seq1.map { x => val dif = x.toDouble - mean; dif * dif } .sum
      val stdDev  = math.sqrt(sumSqr / (seq1.size - 1))
      stdDev
    }
  }

  /** Relative standard deviation, aka coefficient of variation. */
  case object VariationCoeff extends LocalFunction {
    def apply(win: Slice): Double = {
      if (win.size == 1) return 0.0
      val seq1    = win.sq.map(_.dur)
      // the geometric average is n-th root of the product of the durations
      val mean    = (seq1.sum / seq1.size).toDouble
      val sumSqr  = seq1.map { x => val dif = x.toDouble - mean; dif * dif } .sum
      val stdDev  = math.sqrt(sumSqr / (seq1.size - 1))
      math.min(10000, stdDev / mean)
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
// }
sealed trait LocalFunction extends (Slice => Double)

// object MatchFunction {
  /** The relative match, which is the reciprocal of the relative error. This is limited to 1 per mille
    * relative error, in order not to produce infinitely good matches, which would be bad for aggregation.
    */
  case object MatchRelReciprocal extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      if (eval == target) 1.0 else math.min(1.0, target / (1000 * math.abs(eval - target)))
    }
  }

  case object MatchRelNegative extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      1.0 - (if (eval == target) 0.0 else math.min(1.0, math.abs(eval - target) / (1000 * target)))
    }
  }

  case object MatchAbsReciprocal extends MatchFunction {
    def apply(eval: Double, target: Double): Double = {
      math.min(1.0, 1.0 / (1000 * math.abs(eval - target)))
    }
  }

  case object MatchMin extends MatchFunction {
    def apply(a: Double, b: Double): Double = math.min(a, b)
  }

  case object MatchTimes extends MatchFunction {
    def apply(a: Double, b: Double): Double = a * b
  }

  case object MatchLessThan extends MatchFunction {
    def apply(a: Double, b: Double): Double = if (a < b) 1 else 0
  }

  /** Linearly fades between purely `a` (when `w = 0`) purely `b` (when `w = 1`) */
  case class MatchFadeLin(w: Double = 0.5) extends MatchFunction {
    def apply(a: Double, b: Double): Double = a * (1 - w) + b * w
  }

  case class MatchScale(a: ScaleFunction = ScaleLinLin(), b: ScaleFunction = ScaleLinLin(),
                        combine: MatchFunction = MatchTimes) extends MatchFunction { me =>
    def apply(a: Double, b: Double): Double = combine(me.a(a), me.b(b))
  }
// }
sealed trait MatchFunction  extends ((Double, Double) => Double)

// object AggregateFunction {
  case object AggrMean extends AggregateFunction {
    def apply(fits: Vec[Double]): Double = fits.sum / fits.size
  }

  case object AggrRMS extends AggregateFunction {
    def apply(fits: Vec[Double]): Double = 1.0 / math.sqrt(fits.map(x => 1.0/(x * x)).sum / fits.size)
  }

case object AggrMin extends AggregateFunction {
  def apply(fits: Vec[Double]): Double = fits.min
}

// }
sealed trait AggregateFunction extends (Vec[Double] => Double)

case class ScaleLinLin(srcLo: Double = 0, srcHi: Double = 1, dstLo: Double = 0, dstHi: Double = 1) extends ScaleFunction {
  override def apply(in: Double): Double = in.linlin(srcLo, srcHi, dstLo, dstHi)
}
case class ScaleLinExp(srcLo: Double = 0, srcHi: Double = 1, dstLo: Double = 0.1, dstHi: Double = 1) extends ScaleFunction {
  override def apply(in: Double): Double = in.linexp(srcLo, srcHi, dstLo, dstHi)
}
case class ScaleExpLin(srcLo: Double = 0, srcHi: Double = 1, dstLo: Double = 0.1, dstHi: Double = 1) extends ScaleFunction {
  override def apply(in: Double): Double = in.explin(srcLo, srcHi, dstLo, dstHi)
}
sealed trait ScaleFunction extends (Double => Double)