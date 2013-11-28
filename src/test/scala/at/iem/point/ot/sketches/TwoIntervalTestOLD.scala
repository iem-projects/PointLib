package at.iem.point.ot.sketches

import de.sciss.poirot._
import org.jacop.set.search.IndomainSetRandom

/** This test doesn't work. I haven't fully understood how to handle set variables. */
object TwoIntervalTestOLD extends App {
    implicit val m  = Model()
    val s1          = new SetVar(0, 3)
    val s2          = new SetVar(0, 3)
    val union       = s1 * s2
    val zero        = new SetVar(0, 0)
    union <> zero
    val solutionsB  = Vec.newBuilder[(Int, Int)]

    def mkSolution(): Unit = {
      assert(s1.singleton() && s2.singleton())
      // val v1 = s1.domain.lub().min()
      // val v2 = s2.domain.lub().min() // .valueEnumeration.nextElement()
      // val v1 = s1.domain.valueEnumeration().asInstanceOf[SetDomainValueEnumeration].nextSetElement().min()  // .value()
      // val v2 = s2.domain.valueEnumeration().asInstanceOf[SetDomainValueEnumeration].nextSetElement().min()  // .value()
      println(s"---- s1: $s1")
      println(s"---- s2: $s2")
      val v1 = s1.domain.lub().min()
      val v2 = s1.domain.lub().min()
      solutionsB += ((v1, v2))
    }

    val select      = search(List(s1, s2), firstFail, new IndomainSetRandom)
    val result      = satisfyAll[SetVar](select, mkSolution)
    val solutions   = solutionsB.result()

    require(result, s"Constraints could not be satisfied")
    // println(s"num-solutions ${solutions.size}")

    println(s"Found ${solutions.size} solutions:")
    solutions.foreach(println)
}
