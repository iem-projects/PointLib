package at.iem.point.sh.sketches

object BoundaryVersionsApp extends App {
  import Fitness._

  implicit val r = rng(3L)
  for (i <- 1 to 10) {
    val sq = randomSequence(4)
    println("\nInput sequence:")
    println(sq)
    val sqn = sq.map(_.normalized)
    println(sqn)
    // val sq = Vector(norm(0), norm(1), norm(3))
    val flat    = sq.flattenCells
    val flatDur = flat.map(_.dur).sum
    val tol     = flatDur / 5
    val vr = boundaryVersions(flat, drop = 0.75, durTol = tol)
    if (vr.size > 100) {
      println("Wooop. That jumped the fence...")
    } else {
      println(s"${vr.size - 1} new boundary versions${if (vr.size > 1) ":" else ""}")
      vr.filterNot(_ == sqn).foreach(println)
    }
  }
}