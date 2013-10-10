package at.iem.point.sh.sketches

import at.iem.point.sh.sketches.genetic.GlobalImpl

object SlidingWindowTest extends App {
  import Fitness._

  implicit val r = rng(0L)
  val glob    = GlobalImpl(crochets = 8 * 4)
  val sq      = randomSequence(glob)
  val slides  = slideByEvents(5, 2)(sq)
  slides.foreach(println)
}