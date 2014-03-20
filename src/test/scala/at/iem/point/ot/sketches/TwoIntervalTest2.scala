package at.iem.point.ot.sketches

import de.sciss.poirot._
import Implicits._
import de.sciss.kollflitz.Ops._

/** This test verifies the approach of forbidding the appearance of two successive intervals in a chord. */
object TwoIntervalTest2 extends App {
  type Chord          = Vec[IntVar]
  type ChordSolution  = Vec[Int]

  def mkChord1(vc: Int = 3, lo: Int = 0, hi: Int = 4)(implicit m: Model): Chord = {
    val c = Vec.tabulate(vc)(i => IntVar(s"p${vc - i}", lo, hi))
    c.pairMap((h, l) => l #< h)
    c
  }

  def mkChord2(vc: Int = 3, lo: Int = 0, hi: Int = 4)(implicit m: Model): Chord = {
    val c = Vec.tabulate(vc) { i =>
      val isFirst = i == 0
      val hi1 = if (isFirst) hi + 12 else hi - 1
      val res = IntVar(s"p${vc - i}", lo, hi1)
      if (isFirst) {
        (hi until hi1).foreach(res #!= _)
      }
      res
    }
    c.pairMap((h, l) => l #< h)
    c
  }

  // successive: from top to bottom
  def forbidIntervals(c: Chord, successive: Vec[Int])(implicit m: Model): Unit = {
    val numIvals = successive.size
    val chordSz  = numIvals + 1
    require(c.size >= chordSz)

    val acc = successive.reverse.integrate
    println(s"Acc: $acc")

    c.combinations(chordSz).foreach { case upper :+ lo =>
      upper.permutations.foreach { upperP =>
        assert(upperP.size == acc.size)
        println("Group:")
        val ivalsFound = (upperP zip acc).map { case (u, a) =>
          println(s"Ival $lo - $u against $a")
          val ival = (u - lo) % 12
          BooleanVar { ival #= a }
        }
        // val sum = mod.reduce(_ + _)
        // sum #!= 0
        val allIvalsFounds = ivalsFound.reduce(_ & _)
        allIvalsFounds #= false
      }
    }
  }

  locally {
    implicit val m = Model()
    val c = mkChord1()
    go(c)
  }

  locally {
    implicit val m = Model()
    val c = mkChord2()
    go(c)
  }

  def go(c: Chord)(implicit m: Model): Unit = {
    println(s"\n\n--------For chord $c\n")
    forbidIntervals(c, Vec(2, 1))

    val solutionsB  = Vec.newBuilder[ChordSolution]

    def addSolution(): Unit = solutionsB += c.map(_.value())

    val select    = search(c, firstFail, indomainMin)
    val result    = satisfyAll(select, addSolution)
    val solutions = solutionsB.result()

    println(s"Found ${solutions.size} solutions:")  // for c1, should be 8 solutions
    solutions.foreach { c =>
      // allowed  : (0, 1, 2), (0, 1, 4), (0, 2, 3), (0, 2, 4),
      //            (0, 3, 4), (1, 2, 3), (1, 3, 4), (2, 3, 4)
      // forbidden: (0, 1, 3), (1, 2, 4)
      println(c.reverse.mkString(", "))
    }
  }
}
