package at.iem.point.eh.sketches

import de.sciss.contextsnake.ContextTree
import scala.util.Try

object FuzzyDance {
  var DEBUG     = false
  var DEBUG2    = false

  def apply[A](corpus: Traversable[A])
              (init  : Traversable[A] = corpus.take(1))
              (expand: Vec[A] => Iterator[Traversable[A]])
              (implicit random: util.Random): ContextDance[A] = {
    val tree      = ContextTree(corpus.toSeq: _*)
    val snake     = tree.snake(init)
    if (DEBUG) println(s"Begin with $init")
    new Impl[A](tree, snake, random, expand)
  }

  //  def move[A](corpus: Traversable[A], num: Int = 100)
  //             (init  : Traversable[A] = corpus.take(1))(implicit random: util.Random): Vector[A] =
  //    apply(corpus)(init).move(num)

  private final class Impl[A](tree: ContextTree[A], private var snake: ContextTree.Snake[A], rnd: util.Random,
                              expand: Vec[A] => Iterator[Traversable[A]])
    extends ContextDance[A] {

    private var back      = Vector.empty[Set[Vec[A]]]
    private var situation = snake.to[Vector]

    def move(num: Int = 100): Vector[A] = {
      var res       = Vector.empty[A]

      while (res.size /* + snake.size */ < num && snake.nonEmpty) {
        val snakeV  = snake.to[Vector]
        val fuzzy   = expand(snakeV)
        // val succ  = snake.successors.to[Vector]
        val succ: Vec[Vec[A]] = fuzzy.flatMap { v =>
          val subOpt = Try(tree.snake(v)).toOption
          subOpt.fold(Vec.empty[Vec[A]])(_.successors.map { succ => v.toIndexedSeq :+ succ } .toIndexedSeq)
        } .toIndexedSeq

        if (DEBUG2) println(s"Snake ${snake.to[Vector].mkString(", ")}, succ ${succ.mkString(", ")}")
        if (succ.size > 1) {  // grow tail
        val idx     = rnd.nextInt(succ.size)
          val choice  = succ(idx)
          val altern  = succ.toSet - choice
          if (DEBUG)  println(s"Forward with $choice, leaving ${altern.mkString(", ")}")
          // snake      += choice
          snake       = tree.snake(choice)
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
            snake = tree.snake(snake.to[Vector].init) // XXX TODO: bug in ContextTree.Snake ?
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
              // snake      += choice
              snake = tree.snake(choice)
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

      //      if (DEBUG) println(s"Append complete current snake ${snake.to[Vector].mkString(", ")}")
      //      res ++= snake.to[Vector]
      if (DEBUG) println(s"Take $num out of ${res.size}")
      res   = res.take(num)
      res
    }
  }
}