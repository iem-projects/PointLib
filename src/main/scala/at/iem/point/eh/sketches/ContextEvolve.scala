package at.iem.point.eh.sketches

import de.sciss.contextsnake.ContextTree
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}

object ContextEvolve extends App {
  // with start 0, seeds of 1, 2 creates funny loops; 0 and 3 have many walks, 4 is great because it keeps looping but then escapes
  val NUM       = 200
  val SEED      = 4L      // seed of the random number generator
  val START     = 0       // start index in the pitch sequence to begin wih
  val DEBUG     = false
  val DEBUG2    = false

  val sq        = loadSnippet(improvSnippets.last)
  val pitchSq   = sq.notes.map(_.pitch.midi)
  val tree      = ContextTree(pitchSq: _*)
  val start     = pitchSq(START)

  var snake     = tree.snake(start :: Nil)
  var res       = Vector.empty[Int]

  var back      = Vector.empty[Set[Int]]
  val rnd       = new util.Random(SEED)

  var situation = Vector(start)
  if (DEBUG) println(s"Begin with $start")

  while (res.size + snake.size < NUM && snake.nonEmpty) {
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
        assert(snake.nonEmpty)
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
      val (past, future) = situation.splitAt(situation.size / 2)
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
  if (DEBUG) println(s"Take $NUM out of ${res.size}")
  res   = res.take(NUM)

  println(res.map(_.asPitch).mkString(", "))

  val notes = res.zipWithIndex.map { case (midi, idx) =>
    val off = idx * 0.25
    OffsetNote(off, midi.asPitch, 0.125, 80)
  }
  implicit val rate = TickRate.tempo(120, 1024)
  val events  = notes.flatMap(_.toMIDI())
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath / s"Improv_PitchSeq_${START}_$SEED.mid")

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}