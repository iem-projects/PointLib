package at.iem.point.sh.sketches

import spire.math.Rational
import scala.util.Random
import collection.immutable.{IndexedSeq => IIdxSeq}
import scala.annotation.tailrec
import at.iem.point.illism.rhythm.{Note, Cell, NoteOrRest}
import spire.syntax._

object Fitness {
  type Sequence       = IIdxSeq[NoteOrRest]
  type Chromosome     = IIdxSeq[Cell]
  type Genome         = IIdxSeq[Chromosome]
  type GenomeVal      = IIdxSeq[(Chromosome, Double)]

  var showLog = true

  val corpus: IIdxSeq[Cell] = baseCells.flatMap(c => factors.map(c * _))
  val norm  : IIdxSeq[Cell] = corpus.map(_.normalized)

  def log(what: => String) {
    if (showLog) println(s"<ga> $what")
  }

  def rng(seed: Long) = new Random(seed)

  def produce(duration: Rational, iter: Int, pop: Int)
             (fitness: Chromosome => Double, selectAndBreed: GenomeVal => Genome)
             (implicit rnd: Random): GenomeVal = {

    @tailrec def loop(g: Genome, it: Int): Genome = if (it <= 0) g else {
      val g1 = iterate(g, fitness, selectAndBreed)
      loop(g1, it - 1)
    }

    val p0  = Vector.fill(pop)(randomSequence(duration))
    val res = loop(p0, iter)
    weigh(res)(fitness)
  }

  def weigh(pop: Genome)(fitness: Chromosome => Double): GenomeVal = pop.map(seq => seq -> fitness(seq))

  def iterate(pop: Genome, fitness: Chromosome => Double, selectAndBreed: GenomeVal => Genome): Genome = {
    val weighted  = weigh(pop)(fitness)
    val res       = selectAndBreed(weighted)
    log(s"iterate(pop = ${pop.idString}) = ${res.idString}")
    res
  }

  def elitism(sz: Int)(selectAndBreed: GenomeVal => Genome)(g: GenomeVal): Genome = {
    val sorted = g.sortBy(_._2) // highest fitness = last
    val (a, b) = sorted.splitAt(sorted.size - sz)
    selectAndBreed(a) ++ b.drop_2
  }

  def truncationSelection(p: Double)(seq: GenomeVal): Genome = {
    val sorted  = seq.sortBy(_._2)
    val sz      = (seq.size * p + 0.5).toInt
    sorted.takeRight(sz).map(_._1)
  }

  //  def truncationSelection(p: Double)(seq: GenomeVal): Genome = {
  //    val sum     = seq.map(_._2).sum
  //    val norm    = 1.0 / sum
  //    val normed  = seq.map { case (c, w) => c -> w * norm }
  //
  //  }

  def slidingDuration(window: Rational, step: Rational)(fun: (Sequence, Double) => Double)
                     (aggr: IIdxSeq[Double] => Double)
                     (seq: Chromosome): Double = {

    require(step > 0 && window >= step)
    require(seq.nonEmpty)

    val flat        = seq.flattenCells
    val zipped      = flat.accumSeqDur
    val totalDur    = zipped.last._2
    val w1          = totalDur - window
    val w2          = w1.toDouble

    @tailrec def loop(xs: IIdxSeq[(NoteOrRest, Rational)], start: Rational, res: IIdxSeq[Double]): IIdxSeq[Double] = {
      val stop    = start + window
      val slice0  = xs.takeWhile(_._2 <= stop)
      val slice   = (if (slice0.isEmpty) xs.take(1) else slice0).drop_2
      assert(slice.nonEmpty)
      val w       = math.min(1.0, start.toDouble / w2)
      val m       = fun(slice, w)
      val res1    = res :+ m
      val start1  = start + step
      if (start1 < w1) {
        val tail0 = xs.dropWhile(_._2 < start1)
        val tail  = if (tail0.size == xs.size) xs.tail else tail0
        if (tail.nonEmpty) loop(tail, start1, res1) else res1
      } else res1
    }

    val m = loop(zipped, r"0", Vector.empty)
    aggr(m)
  }

  implicit final class RichIndexedSeq[A](val seq: IIdxSeq[A]) extends AnyVal {
    def choose()(implicit rnd: Random): A = seq(rnd.nextInt(seq.size))
    def flattenCells(implicit ev: A <:< Cell): Sequence = seq.flatMap(c => ev(c).normalized.elements)
    def drop_2[B](implicit ev: A <:< (B, _)): IIdxSeq[B] = seq.map(_._1)

    //    def toCell(implicit ev: A <:< Rational): Cell = {
    //      val elems = seq.map(Note(_))
    //      val dur   = elems.map(_.dur).sum
    //      Cell(-1, elems, dur)
    //    }

    def toCell(implicit ev: A <:< NoteOrRest): Cell = {
      val elems = seq.map(ev(_))
      val dur   = elems.map(_.dur).sum
      Cell(-1, elems, dur)
    }

    def idString(implicit ev: A <:< Chromosome): String =
      seq.map(ev(_).map(_.id).mkString("<", ",", ">")).mkString("[", ", ", "]")

    def dur(implicit ev: A <:< Cell): Rational = seq.map(ev(_).dur).sum

    def accumDur(implicit ev: A <:< Cell): IIdxSeq[(A, Rational)] =
      seq zip seq.scanLeft(r"0")(_ + ev(_).dur).tail

    def accumSeqDur(implicit ev: A <:< NoteOrRest): IIdxSeq[(A, Rational)] =
      seq zip seq.scanLeft(r"0")(_ + ev(_).dur).tail
  }

  def randomSequence(duration: Rational)(implicit rnd: Random): Chromosome = {
    @tailrec def loop(seq: Chromosome, d: Rational): IIdxSeq[Cell] = {
      val c     = corpus.choose()
      val sqn   = seq :+ c
      val dn    = d + c.dur
      if (dn >= duration) sqn else loop(sqn, dn)
    }

    loop(Vector.empty, 0)
  }

  def boundaryVersions(seq: Sequence, drop: Double = 0, durTol: Rational = 0)
                      (implicit rnd: Random): Genome = {
    def loop(xs: Sequence): Genome = {
      val filter = norm.filter { c =>
        if (c.size > xs.size) {
          val over = c.elements.takeRight(c.size - xs.size).map(_.dur).sum
          if (over <= durTol) {
            val c1  = c.elements.take(xs.size)
            val ys  = xs.take(c1.size)
            ys == c1
          } else {
            false
          }
        } else {
          val ys = xs.take(c.size)
          ys == c.elements
        }
      }
      val res1 = filter.flatMap { c =>
        val tail    = xs.drop(c.size)
        if (tail.isEmpty) Vector(Vector(c)) else {
          val rest   = loop(tail)
          val withC  = rest.map(c +: _)
          val tailDur = tail.map(_.dur).sum
          if (tailDur < tailDur) Vector(c) +: withC else withC
        }
      }
      if (xs.isEmpty || rnd.nextDouble() >= drop) res1 else res1 ++ loop(xs.tail)
    }

    loop(seq)
  }
}