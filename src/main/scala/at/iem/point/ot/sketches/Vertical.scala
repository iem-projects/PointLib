package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.util.Random
import collection.breakOut
import scala.annotation.tailrec
import de.sciss.midi
import de.sciss.file._
import java.io.{OutputStreamWriter, FileOutputStream}
import de.sciss.kollflitz.RandomOps._

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

  lazy val allIntervals       : Vec[Interval] = chords.flatten.flatMap(c =>  c.allIntervals    )(breakOut)
  lazy val layeredIntervals   : Vec[Interval] = chords.flatten.flatMap(c =>  c.layeredIntervals)(breakOut)
  lazy val coAllIntervals     : Map[Interval, Vec[Interval]] = mkCoIntervals(_.allIntervals    )
  lazy val coLayeredIntervals : Map[Interval, Vec[Interval]] = mkCoIntervals(_.layeredIntervals)

  private def mkCoIntervals(fun: Chord => Vec[Interval]): Map[Interval, Vec[Interval]] = {
    var mp  = Map.empty[Interval, Vec[Interval]]
    val cs  = chords.flatten
    cs.foreach { c =>
      val iv = fun(c)
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

    val ivalSeq = if (useAllIntervals)   allIntervals else   layeredIntervals
    val ivalMap = if (useAllIntervals) coAllIntervals else coLayeredIntervals

    if (voices == 1) {
      mkChord(Vec.empty)
    } else {
      @tailrec def loopChord(n: Int, pred: Interval, res: Vec[Interval]): Vec[Interval] =
        if (n == 0) res else {
          val succ = ivalMap(pred).choose()
          loopChord(n - 1, succ, res :+ succ)
        }

      @tailrec def loop(): Chord = {
        val ival1   = ivalSeq.choose()
        val ivals   = loopChord(voices - 2, ival1, Vec(ival1))
        val c       = mkChord(ivals)
        val test    = c.allIntervals
        if (test.forall(coAllIntervals.contains)) c else loop()
      }

      loop()
    }
  }

  // ---- application ----
  run(useAllIntervals = true)

  /** @param voices           the number of voices of the chords
    * @param num              the number of chords to produce
    * @param base             the base pitch for each chord
    * @param useAllIntervals  if `true`, use all intervals as pool, otherwise use layered intervals as pool
    */
  def run(voices: Int = 5, num: Int = 48, base: Pitch = 60.asPitch, useAllIntervals: Boolean = false): Unit = {
    implicit val random = mkRandom()
    implicit val ticks  = midi.TickRate.tempo(120, 1024)
    val fb      = outDir / s"vertical_gen$voices${if (useAllIntervals) "all" else ""}"
    val chords0 = Vec.fill(num)(generate(voices = voices, base = base, useAllIntervals = useAllIntervals))
    val chords  = chords0.spread()

    val fmid    = fb.replaceExt("midi")
    val evt     = chords.flatMap(_.toMIDI(0))
    val t       = midi.Track(evt)
    midi.Sequence(Vec(t)).writeFile(fmid)

    val fly     = fb.replaceExt("ly")

    // ---- lilypond test output ----
    val headly = raw"""
      |\version "2.16.2"
      |
      |#(set-default-paper-size "a4")
      |#(set-global-staff-size 16)
    """.stripMargin

    val innerly = chords.map { c =>
      c.notes.map { n =>
        val p   = n.pitch
        val ps  = Pitch.toString(p.midi, Language.German)
        ps.replace('h', 'b')  // Yo, suckers
      } .mkString("  <", " ", ">1")
    } .mkString("\n")

    val score = raw"""
      |$headly
      |
      |\new Staff {
      |$innerly
      |}
    """.stripMargin

    val os  = new OutputStreamWriter(new FileOutputStream(fly), "UTF-8")
    os.write(score)
    os.close()

    import sys.process._

    val fdir  = fly.parent
    val cmdly = Seq(lilypond, "-o", fdir.path, fly.absolutePath)
    println(cmdly.mkString(" "))
    val resly = cmdly.!
    if (resly != 0 && resly != 1) sys.error(s"Lilypond exited with code $resly")

    Seq(pdfViewer, fly.replaceExt("pdf").path).!
  }
}