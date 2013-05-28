package at.iem.point.sh.sketches

object SlidingWindowTest extends App {
  import Fitness._

  implicit val r = rng(0L)
  val sq = randomSequence(8)
  val slides = slideByEvents(5, 2)(sq)
  slides.foreach(println)
}