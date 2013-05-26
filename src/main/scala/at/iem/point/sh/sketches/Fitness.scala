package at.iem.point.sh.sketches

import spire.math.Rational
import scala.util.Random
import collection.immutable.{IndexedSeq => IIdxSeq}
import scala.annotation.tailrec
import at.iem.point.illism.rhythm.{Cell, NoteOrRest}

object Fitness {
  val corpus: IIdxSeq[Cell] = baseCells.flatMap(c => factors.map(c * _))
  val norm  : IIdxSeq[Cell] = corpus.map(_.normalized)

  def rng(seed: Long) = new Random(seed)

  def produce(duration: Rational, measure: Cell => Double, window: Rational, step: Rational,
              fitness: IIdxSeq[Double] => Double, iter: Int, pop: Int, sel: Int, elitism: Int,
              seed: Long)(implicit rnd: Random): Cell = {

    ???
  }

  implicit final class RichIndexedSeq[A](val seq: IIdxSeq[A]) extends AnyVal {
    def choose()(implicit rnd: Random): A = seq(rnd.nextInt(seq.size))
    def flattenCells(implicit ev: A <:< Cell): IIdxSeq[NoteOrRest] = seq.flatMap(c => ev(c).normalized.elements)
  }

  def randomSequence(duration: Rational)(implicit rnd: Random): IIdxSeq[Cell] = {
    @tailrec def loop(seq: IIdxSeq[Cell], d: Rational): IIdxSeq[Cell] = {
      val c     = corpus.choose()
      val sqn   = seq :+ c
      val dn    = d + c.dur
      if (dn >= duration) sqn else loop(sqn, dn)
    }

    loop(Vector.empty, 0)
  }

  def boundaryVersions(seq: IIdxSeq[NoteOrRest], drop: Double = 0, durTol: Rational = 0)
                      (implicit rnd: Random): IIdxSeq[IIdxSeq[Cell]] = {
    def loop(xs: IIdxSeq[NoteOrRest]): IIdxSeq[IIdxSeq[Cell]] = {
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