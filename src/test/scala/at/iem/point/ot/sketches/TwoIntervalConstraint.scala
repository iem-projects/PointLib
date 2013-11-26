package at.iem.point.ot.sketches

import de.sciss.jacop._
import Implicits._
import JaCoP.search.{Indomain, IndomainRandom, SimpleSelect, SmallestDomain}
import JaCoP.set.search.IndomainSetRandom
import scala.concurrent.JavaConversions
import JaCoP.set.core.SetDomainValueEnumeration

object TwoIntervalConstraint extends App {
  implicit val m  = new Model()
  val s1          = new SetVar(0, 3)
  val s2          = new SetVar(0, 3)
  val union       = s1 * s2
  val zero        = new SetVar(0, 0)
  union <> zero
  val solutionsB  = Vec.newBuilder[(Int, Int)]

  def mkSolution(): (Int, Int) = {
    assert(s1.singleton() && s2.singleton())
    // val v1 = s1.domain.lub().min()
    // val v2 = s2.domain.lub().min() // .valueEnumeration.nextElement()
    // val v1 = s1.domain.valueEnumeration().asInstanceOf[SetDomainValueEnumeration].nextSetElement().min()  // .value()
    // val v2 = s2.domain.valueEnumeration().asInstanceOf[SetDomainValueEnumeration].nextSetElement().min()  // .value()
    println(s"---- s1: $s1")
    println(s"---- s2: $s2")
    val v1 = s1.domain.lub().min()
    val v2 = s1.domain.lub().min()
    (v1, v2)
  }

  val select      = new SimpleSelect[SetVar](Array(s1, s2), new SmallestDomain[SetVar](), new IndomainSetRandom)
  val result      = satisfyAll[SetVar](select, () => solutionsB += mkSolution())
  val solutions   = solutionsB.result()

  require(result, s"Constraints could not be satisfied")
  // println(s"num-solutions ${solutions.size}")

  println(s"Found ${solutions.size} solutions:")
  solutions.foreach(println)
}
