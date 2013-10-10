package at.iem.point.sh.sketches
package genetic

import Fitness._
import spire.math.Rational
import collection.immutable.{IndexedSeq => Vec}
import at.iem.point.sh.sketches.genetic.GeneticSystem.{Global, Chromosome}
import de.sciss.muta
import de.sciss.muta.{SelectionPercent, SelectionNumber, SelectionSize}
import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format

/** A type describing the breeding procedure.
  *
  * @param elitism          the amount of best chromosomes kept unmodified
  * @param crossoverWeight  the balance between pure crossover (100) and pure mutation (0)
  * @param crossover        the crossover breeding function, applied with the selected chromosomes and number of children to produce
  * @param mutation         the mutation  breeding function, applied with the selected chromosomes and number of children to produce
  */
case class BreedingImpl(elitism: SelectionSize = /* SelectionSize. */ SelectionNumber(5),
                    crossoverWeight: SelectionPercent = SelectionPercent(80),
                    crossover: BreedingFunction = /* BreedingFunction. */ OnePointCrossover,
                    mutation : BreedingFunction = /* BreedingFunction. */ SingleCellMutation)
  extends muta.Breeding[Chromosome, Global] { // ((GenomeSel, Rational, util.Random) => Genome) {

  override def apply(g: GenomeSel, global: Global, r: util.Random): Genome = {
    val szOut = g.size
    val szEl  = elitism(szOut)
    val out1  = if (szEl == 0) Vec.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = g.map { case (c, f, _) => (c, f) } .distinct.sortBy(-_._2).map(_._1)
      eliteCandidates.take(szEl)
    }
    val szBr  = szOut - out1.size
    val szX   = crossoverWeight(szBr)
    val szMut = szBr - szX
    val sel   = g.collect {
      case (c, _, true) => c
    }
    val out2  = if (szX   == 0) out1 else out1 ++ crossover(sel, szX  , global, r)
    val out3  = if (szMut == 0) out2 else out2 ++ mutation (sel, szMut, global, r)
    out3
  }
}

// ----------------------------------------------------------------

case object OnePointCrossover extends BreedingFunction {
  def apply(g: Genome, num: Int, global: Global, r: util.Random): Genome = {
    val res = Vec.newBuilder[Chromosome]
    res.sizeHint(num)
    var sz  = 0
    val urn = new Urn(g)
    while (sz < num) {
      val e1  = urn()(r)
      val e2  = urn()(r)
      res    += cross(e1, e2, global, r)
      sz     += 1
      if (sz < num) {
        res  += cross(e2, e1, global, r)
        sz   += 1
      }
    }
    res.result()
  }

  private def cross(e1: Chromosome, e2: Chromosome, global: Global, r: util.Random): Chromosome = {
    val dur   = global.duration
    val i     = r.nextInt(e1.size)  // splitting point
    val s1    = e1.take(i)          // left hand side
    val d1    = s1.dur
    val e2d   = e2.dur
    val fill  = dur - d1       // optimum duration of right-hand side
    val miss  = fill - e2d
    if (miss > 0) { // s1 (splitted e1) plus full e2 still too short, thus grow s1 and append e2
      val r1  = e1.drop(i).accumDur
      val t1  = r1.takeWhile { case (c, acc) => acc - c.dur < miss }
      val s1b = t1.optimumEnd(miss)(_._2) .drop_2
      s1b ++ e2

    } else if (fill == 0) { // s1 has perfect length
      s1

    } else {  // find s2, the optimium truncation of e2 at its beginning, and prepend s1

      val e2r   = e2.reverse
      val e2d   = e2r.accumDur
      val s2a   = e2d.takeWhile { case (n, acc) => acc - n.dur < fill }
      val s2    = s2a.optimumEnd(fill)(_._2) .drop_2.reverse  // optimumEnd on reversed seq is actually an optimum start
      s1 ++ s2
    }
  }
}

/** Mutates by dropping a random cell from the chromosome, and filling up random spots of the chromosome
  * until the target duration is reached again.
  */
case object SingleCellMutation extends BreedingFunction {
  override def apply(g: Genome, num: Int, global: Global, r: util.Random): Genome =
    applyN(1)(g, num, global, r)

  private[genetic] def applyN(numMut: Int)(g: Genome, num: Int, global: Global, r: util.Random): Genome = {
    val res = Vec.newBuilder[Chromosome]
    res.sizeHint(num)
    var sz  = 0
    val urn = new Urn(g)
    val dur = global.duration
    while (sz < num) {
      val e1  = urn()(r)
      var k   = 0
      var e3  = e1
      while (k < numMut) {
        val i   = r.nextInt(e1.size)      // removal index
        val e2  = e1.removeAt(i)
        e3      = if (e2.dur >= dur) e2 else {
          val j   = r.nextInt(e2.size + 1)  // insertion point
          var t1  = e2
          var t2  = e2
          while (t2.dur < dur) {
            t1 = t2
            t2 = t2.insertAt(j, global.corpus.choose()(r))
          }
          if (t1.isEmpty) t2 else optimize(dur, t1, t2)(_.dur)
        }
        k += 1
      }
      res += e3
      sz  += 1
    }
    res.result()
  }
}

case class MultiCellMutation(n: Int = 2) extends BreedingFunction {
  override def apply(g: Genome, num: Int, global: Global, r: util.Random): Genome =
    SingleCellMutation.applyN(n)(g, num, global, r)
}

case class SwapMutation(n: Int = 2) extends BreedingFunction {
  override def apply(g: Genome, num: Int, global: Global, r: util.Random): Genome = {
    val res = Vec.newBuilder[Chromosome]
    res.sizeHint(num)
    var sz  = 0
    val urn = new Urn(g)
    while (sz < num) {
      val e1  = urn()(r)
      var k   = 0
      var e3  = e1
      while (k < n) {
        val i   = r.nextInt(e1.size)      // removal index
        val c   = e1(i)
        val e2  = e1.removeAt(i)
        val j   = r.nextInt(e2.size + 1)  // insertion point
        e3      = e2.insertAt(j, c)
        k += 1
      }
      res += e3
      sz  += 1
    }
    res.result()
  }
}

case class CombinedMutation(a: BreedingFunction = MultiCellMutation(),
                            b: BreedingFunction = SwapMutation(), balance: SelectionPercent = SelectionPercent(50))
  extends BreedingFunction {

  override def apply(g: Genome, num: Int, global: Global, r: util.Random): Genome = {
    val bNum  = balance(num)
    val aNum  = num - bNum
    val aGen  = a(g, aNum, global, r)
    val bGen  = a(g, aNum, global, r)
    aGen ++ bGen
  }
}

sealed trait BreedingFunction extends ((Genome, Int, Global, util.Random) => Genome)

object BreedingFunction {
  implicit val format: Format[BreedingFunction] = AutoFormat[BreedingFunction]
}