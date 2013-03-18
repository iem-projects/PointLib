package at.iem.point.eh.sketches

import de.sciss.contextsnake.ContextTree

object ContextEvolve extends App {
  val NUM       = 100
  val SEED      = 0L
  val DEBUG     = true

  val sq        = loadSnippet(improvSnippets.last)
  val pitchSq   = sq.notes.map(_.pitch.midi)
  val tree      = ContextTree(pitchSq: _*)
  val start     = pitchSq.head

  var snake     = tree.snake(start :: Nil)
  var res       = Vector.empty[Int]

  var back      = Vector.empty[Set[Int]]
  val rnd       = new util.Random(SEED)

  var situation = Vector(start)
  if (DEBUG) println(s"Begin with $start")

  while (res.size + snake.size < NUM && snake.nonEmpty) {
    val succ    = snake.successors.to[Vector]
    if (succ.size > 1) {  // grow tail
      val idx     = rnd.nextInt(succ.size)
      val choice  = succ(idx)
      val altern  = succ.toSet - choice
      if (DEBUG) println(s"Forward with $choice, leaving ${altern.mkString(", ")}")
      snake      += choice
      back      :+= altern
      if (snake.size > situation.size) {
        situation = snake.to[Vector]
        if (DEBUG) println(s"Situation now ${situation.mkString(", ")}")
      }

    } else if (back.nonEmpty) { // back track (shrink tail)
      snake.trimEnd(1)
      val succ    = back.last.to[Vector]
      if (DEBUG) println(s"Trimming tail. Pop back tracking ${succ.mkString(", ")}")
      back        = back.init
      if (succ.size > 0) {
        val idx     = rnd.nextInt(succ.size)
        val choice  = succ(idx)
        val altern  = succ.toSet - choice
        if (DEBUG) println(s"Reenter with $choice, leaving ${altern.mkString(", ")}")
        snake      += choice
        /* if (altern.nonEmpty) */ back :+= altern
      }

    } else {  // shrink head
      // ignore current snake body but remember longest streak so far (`situation`)
      val (past, future) = situation.splitAt(situation.size / 2)
      if (DEBUG) println(s"Splitting situation, copying ${past.mkString(", ")}, new body ${future.mkString(", ")}")
      res     ++= past
      snake     = tree.snake(future)  // reset snake
      back      = Vector.empty        // TODO: this could be richer
      situation = future
    }
  }

  if (DEBUG) println(s"Append complete current snake ${snake.to[Vector].mkString(", ")}")
  res ++= snake.to[Vector]
  if (DEBUG) println(s"Take $NUM out of ${res.size}")
  res   = res.take(NUM)

  println(res.mkString(", "))
}