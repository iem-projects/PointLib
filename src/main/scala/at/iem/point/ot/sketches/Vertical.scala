package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.util.Random
import collection.breakOut
import scala.annotation.tailrec
import de.sciss.midi
import de.sciss.file._

object Vertical extends App {
  // - erlaubte intervalle
  // - erlaubte intervall konstellationen
  // - was ist mit register? gehoert zur stimmfuehrung? (ja)

  // algorithmus:
  // - gegeben grundton
  // - gegeben N zahl der stimmen
  // - generiere N-1 intervalle

  // das erste intervall koennte aus dem pool aller vorkommenden intervalle genommen werden
  // (a) zufaellig; (b) zufaellig aber gewichtet nach frequenz
  // das nachfolgende intervall wuerde aus pool der xkorr genommen werden
  // (a) zufaellig; (b) zufaellig aber gewichtet nach frequenz

  // dann wuerden akkorde weggeworfen werden muessen, wenn all-intervalle dadurch entstehen, die nicht im korpus sind

  lazy val allIntervals: Vec[Interval] = chords.flatten.flatMap(c => c.allIntervals)(breakOut)

  lazy val coIntervals = {
    var mp  = Map.empty[Interval, Vec[Interval]]
    val cs  = chords.flatten
    cs.foreach { c =>
      val iv = c.allIntervals
      for {
        (i,ii) <- iv.zipWithIndex
        (j,jj) <- iv.zipWithIndex
        if ii < jj
      } {
        val im = mp.getOrElse(i, Vec.empty)
        val jm = mp.getOrElse(j, Vec.empty)
        mp += i -> (im :+ j)
        mp += j -> (jm :+ i)
      }
    }
    mp
  }

  def generate(voices: Int, base: Pitch = 60.asPitch, useAllIntervals: Boolean = false)
              (implicit random: Random): Chord = {
    require(voices > 0, s"Voices ($voices) must be >0")

    def mkChord(ivals: Vec[Interval]) = {
      val pitches = ivals.scanLeft(base)(_ + _)
      val notes   = pitches.map(pitch => OffsetNote(offset = 0, pitch = pitch, duration = 1, velocity = 80))
      //try {
      Chord(notes)
      //} catch {
      //  case e @ util.control.NonFatalNonFatal(_) => println(ivals); throw e
      //}
    }

    if (voices == 1) {
      mkChord(Vec.empty)
    } else {
      @tailrec def loopChord(n: Int, pred: Interval, res: Vec[Interval]): Vec[Interval] =
        if (n == 0) res else {
          val succ = coIntervals(pred).choose
          loopChord(n - 1, succ, res :+ succ)
        }

      @tailrec def loop(): Chord = {
        val ival1   = allIntervals.choose
        val ivals   = loopChord(voices - 2, ival1, Vec(ival1))
        val c       = mkChord(ivals)
        val test    = c.allIntervals
        if (test.forall(coIntervals.contains)) c else loop()
      }

      loop()
    }
  }

  // ---- application ----
  run()

  def run(voices: Int = 5, num: Int = 48, base: Pitch = 60.asPitch, useAllIntervals: Boolean = false) {
    implicit val random = mkRandom()
    implicit val ticks  = midi.TickRate.tempo(120, 1024)
    val f       = outDir / s"vertical_gen$voices${if (useAllIntervals) "all" else ""}}.midi"
    val chords0 = Vec.fill(num)(generate(voices = voices, base = base, useAllIntervals = useAllIntervals))
    val chords  = chords0.spread()
    val evt     = chords.flatMap(_.toMIDI(0))
    val t       = midi.Track(evt)
    midi.Sequence(Vec(t)).writeFile(f)
  }
}