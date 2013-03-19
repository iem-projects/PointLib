package at.iem.point.eh.sketches

import de.sciss.contextsnake.ContextTree

object ContextDance {
  var DEBUG     = false
  var DEBUG2    = false

  def move[A](corpus: Traversable[A], num: Int = 100, seed: Long = 0L)
             (init: Traversable[A] = corpus.take(1)): Vector[A] = {

    val tree      = ContextTree(corpus.toSeq: _*)
    var snake     = tree.snake(init)
    var res       = Vector.empty[A]

    var back      = Vector.empty[Set[A]]
    val rnd       = new util.Random(seed)

    var situation = snake.to[Vector]
    if (DEBUG) println(s"Begin with $init")

    while (res.size + snake.size < num && snake.nonEmpty) {
      val succ = snake.successors.to[Vector]
      if (DEBUG2) println(s"Snake ${snake.to[Vector].mkString(", ")}, succ ${succ.mkString(", ")}")
      if (succ.size > 1) {  // grow tail
        val idx     = rnd.nextInt(succ.size)
        val choice  = succ(idx)
        val altern  = succ.toSet - choice
        if (DEBUG)  println(s"Forward with $choice, leaving ${altern.mkString(", ")}")
        snake      += choice
        if (DEBUG2) println(s"Snake add $choice, yields ${snake.to[Vector].mkString(", ")}")
        back      :+= altern
        if (DEBUG2) println(s"Back ${back.mkString(", ")}")
        if (snake.size > situation.size) {
          situation = snake.to[Vector]
          if (DEBUG || DEBUG2) println(s"Situation now ${situation.mkString(", ")}")
        }

      } else {  // back track (shrink tail)
        var foundAltern = false
        while (back.nonEmpty && !foundAltern) {
//          assert(snake.nonEmpty)
  //        if (snake.size == 2) {
  //          println("aqui")
  //        }
  //        snake.trimEnd(1)
  snake = tree.snake(snake.to[Vector].init) // XXX TOOD: bug in ContextTree.Snake ?
          if (DEBUG2) println(s"Snake trimmed to ${snake.to[Vector].mkString(", ")}")
          val succ    = back.last.to[Vector]
          if (DEBUG || DEBUG2) println(s"Trimming tail. Pop back tracking ${succ.mkString(", ")}")
          back        = back.init
          if (DEBUG2) println(s"Back trimmed to $back")
          if (succ.size > 0) {
            val idx     = rnd.nextInt(succ.size)
            val choice  = succ(idx)
            val altern  = succ.toSet - choice
            if (DEBUG) println(s"Reenter with $choice, leaving ${altern.mkString(", ")}")
            snake      += choice
            if (DEBUG2) println(s"Snake add $choice, yields ${snake.to[Vector].mkString(", ")}")
            /* if (altern.nonEmpty) */ back :+= altern
            if (DEBUG2) println(s"Back ${back.mkString(", ")}")
            foundAltern = true
          }
        }

        if (!foundAltern) {  // shrink head
          // ignore current snake body but remember longest streak so far (`situation`)
          val sitSz = situation.size
          val (past, future) = if (sitSz == 1) {
            (situation, tree.snake(situation).successors.to[Vector])  // successors must have size 1 here
          } else {
            situation.splitAt(sitSz / 2)
          }
          if (DEBUG) println(s"Splitting situation, copying ${past.mkString(", ")}, new body ${future.mkString(", ")}")
          res     ++= past
          snake     = tree.snake(future)  // reset snake
          back      = Vector.empty        // TODO: this could be richer
          situation = future
        }
      }
    }

    if (DEBUG) println(s"Append complete current snake ${snake.to[Vector].mkString(", ")}")
    res ++= snake.to[Vector]
    if (DEBUG) println(s"Take $num out of ${res.size}")
    res   = res.take(num)
    res
  }
}