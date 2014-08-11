package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import at.iem.point.illism._
import scala.util.Random
import de.sciss.muta.{SelectionNumber, SelectionPercent, SelectionSize}
import scala.swing.{Component, Label}
import play.api.libs.json.{JsSuccess, JsError, JsString, JsObject, JsResult, JsValue, Format}
import de.sciss.poirot
import de.sciss.numbers
import de.sciss.play.json.AutoFormat
import collection.breakOut
import language.implicitConversions
import de.sciss.poirot.{Model, IntVar}
import de.sciss.kollflitz.Ops._
import de.sciss.kollflitz.RandomOps._

sealed trait FiniteConstraintType
case object Allow  extends FiniteConstraintType
case object Forbid extends FiniteConstraintType
object FiniteConstraintType {
  implicit val format = AutoFormat[FiniteConstraintType]
}

case class VoiceDirection(limit: Int, step: Int = 2, constraint: FiniteConstraintType = Forbid, intervals: String = "")
object VoiceDirection {
  implicit val format = AutoFormat[VoiceDirection]
}

case class Voice(up: VoiceDirection = VoiceDirection(limit = 96), down: VoiceDirection = VoiceDirection(limit = 36))

object Voice {
  implicit val format = AutoFormat[Voice]
}

sealed trait GlobalConstraint {
  def apply(chromosome: Vec[Vec[IntVar]])(implicit m: poirot.Model): Unit
}
case class FirstChord(pitches: String = "") extends GlobalConstraint with GlobalConstraint.ChordImpl {
  protected def pickChord(chromosome: Vec[Vec[IntVar]]): Vec[IntVar] = chromosome.head
}
case class LastChord (pitches: String = "") extends GlobalConstraint with GlobalConstraint.ChordImpl {
  protected def pickChord(chromosome: Vec[Vec[IntVar]]): Vec[IntVar] = chromosome.last
}
case class GivenVoice(voice: Int = 1, pitches: String = "") extends GlobalConstraint {
  require(voice >= 1, s"Voice index ($voice) must be >= 1")
  private val pitchesI = GeneticSystem.stringToIntervals(pitches)

  def apply(chromosome: Vec[Vec[IntVar]])(implicit m: Model): Unit = {
    require(chromosome.size == pitchesI.size,
      s"GivenVoice - wrong sequence length (is ${pitchesI.size}, expected ${chromosome.size}")
    if (chromosome.isEmpty) return

    val numVoices = chromosome.head.size
    require(numVoices >= voice, s"GivenVoice - index ($voice) must <= numVoices ($numVoices)")
    val line = chromosome.map(_.apply(numVoices - voice))
    (line zip pitchesI).foreach { case (p, v) =>
      p #= v
    }
  }
}
object GlobalConstraint {
  trait ChordImpl {
    private lazy val pitchesI   = GeneticSystem.stringToIntervals(pitches)
    private lazy val check      = require(pitchesI.isSorted, s"Chord pitches ($pitchesI) must be given in ascending order")
    private lazy val pitchesIR  = pitchesI.reverse

    protected def pitches: String
    protected def pickChord(chromosome: Vec[Vec[IntVar]]): Vec[IntVar]

    def apply(chromosome: Vec[Vec[IntVar]])(implicit m: Model): Unit = {
      check
      val ch = pickChord(chromosome)
      require(ch.size == pitchesI.size,
        s"FirstChord constraint has wrong number of voices (${pitchesI.size}, should be ${ch.size})")
      (ch zip pitchesIR).foreach { case (vr, v) =>
        vr #= v
      }
    }
  }

  implicit val format = AutoFormat[GlobalConstraint]
}

/** The global configuration for the generation of chromosomes.
  *
  * @param voices    the number of voices (vertical chord size)
  * @param vertical  vertical constraints
  * @param length    the number of chords to produce
  */
case class GlobalImpl(voices  : Vec[Voice] = GeneticSystem.DefaultVoices,
                      vertical: Vec[VerticalConstraint] = Vec(ForbiddenInterval(12)),
                      global  : Vec[GlobalConstraint] = Vec.empty,
                      length  : Int = 12)

case class GenerationImpl(size: Int = 100, global: GlobalImpl = GlobalImpl(), seed: Int = 0)
  extends muta.Generation[GeneticSystem.Chromosome, GlobalImpl] {
  
  import GeneticSystem.{Chromosome, constrainHoriz, constrainVertical, pitchesToChord, varToPitch}

  def apply(r: Random): Chromosome = {
    val voices    = global.voices
    val numVoices = voices.size
    val num       = global.length

    import poirot.{Vec => _, _} // IntelliJ freaks out when Vec is shadowed with itself
    implicit val model = Model()
    import Implicits._

    // vertical constraints
    val vars = Vec.fill(num) {
      val ch = Vec.fill(numVoices)(IntVar())
      constrainVertical(ch, voices, global.vertical)
      ch
    }
    // horizontal constraints
    vars.mapPairs { (c1, c2) =>
      constrainHoriz(c1, c2, voices)
    }
    // global constraints
    global.global.foreach(_.apply(vars))

    require(model.consistency(), s"Constraints model is not consistent")

    def mkChromosome(): Chromosome = vars.map { vc =>
      val chord = pitchesToChord(vc)
      (chord, ChordNeutral)
    }

    val select        = search(vars.flatten, firstFail, indomainRandom(r))
    val solutionsB    = Vec.newBuilder[Chromosome]
    maxNumSolutions   = math.max(1, math.min(math.pow(size, 1.5), 16384).toInt)
    timeOut           = GeneticApp.mTimeOut.getValue.asInstanceOf[Int] // 30 // 30    // seconds

    def addSolution(): Unit = solutionsB += mkChromosome()

    val result        = satisfyAll(select, addSolution)
    val solutions     = solutionsB.result()

    require(result, "Constraints could not be satisfied")
    // println(s"num-solutions ${solutions.size}")

    solutions.choose()(r)
  }
}

sealed trait SelectionImpl extends muta.Selection[GeneticSystem.Chromosome]

case class SelectionRoulette(size: SelectionSize = SelectionPercent(33))
  extends muta.impl.SelectionRouletteImpl[GeneticSystem.Chromosome] with SelectionImpl

case class SelectionTruncation(size: SelectionSize = SelectionPercent(20))
  extends muta.impl.SelectionTruncationImpl[GeneticSystem.Chromosome] with SelectionImpl

sealed trait BreedingFunctionImpl extends muta.BreedingFunction[GeneticSystem.Chromosome, GlobalImpl]

case class BreedingImpl(elitism         : SelectionSize         = SelectionNumber(5),
                        crossoverWeight : SelectionPercent      = SelectionPercent(0 /* 50 */),  // crossover doesn't honor constraints yet
                        crossover       : BreedingFunctionImpl  = Crossover,
                        mutation        : BreedingFunctionImpl  = Mutation())
  extends muta.impl.BreedingImpl[GeneticSystem.Chromosome, GlobalImpl]

object Crossover extends /* muta.impl.CrossoverVecImpl[GeneticSystem.Gene, GlobalImpl] with */ BreedingFunctionImpl {
  import GeneticSystem.{Chromosome, Genome, Global}

  private final val NUM_TRIES = 32

  def apply(gen: Genome, num: Int, glob: Global, r: Random): Genome = {
    val gsz = gen.size
    if (gsz < 2) return Vec.fill(num)(gen.head)

    val res = Vec.newBuilder[Chromosome]
    res.sizeHint(num)

    /* @tailrec */ def outerLoop(rem: Int): Unit = if (rem > 0) {
      /* @tailrec */ def innerLoop(tries: Int): Unit =
        if (tries == 0) {
          println(s"Crossover: Giving up...")

        } else {
          val i   = r.nextInt(gen.size)
          val j0  = r.nextInt(gen.size - 1)
          val j   = if (j0 < i) j0 else j0 + 1    // thus, j != i
          val gi  = gen(i)
          val gj  = gen(j)
          val szi = gi.size
          val szj = gj.size
          val len = (szi + szj) / 2
          val li  = r.nextInt(math.min(len, szi))
          val lj  = math.min(szj, len - li)
          val c   = gi.take(li) ++ gj.drop(szj - lj)

          if (GeneticSystem.accept(c, glob)) {
            res += c
          } else {
            innerLoop(tries - 1)
          }
        }

      innerLoop(NUM_TRIES)
      outerLoop(rem - 1)
    }

    outerLoop(num)
    val found = res.result()
    if (found.size > num) found.take(num) else if (found.size == num) found else {
      val pad = Vec.fill(num - found.size)(gen(r.nextInt(gen.size)))
      found ++ pad
    }
  }
}

case class Mutation(chordMin: SelectionSize = SelectionNumber(1), chordMax: SelectionSize = SelectionNumber(5),
                    voiceMin: SelectionSize = SelectionNumber(1), voiceMax: SelectionSize = SelectionNumber(3),
                    excludeVoices: String = "", interval: Int = 7)
  extends BreedingFunctionImpl {

  val excludeVoiceNums: Vec[Int] = excludeVoices.split(' ').filter(_.nonEmpty).map(_.toInt - 1)(breakOut)

  // def numGenesSize = chords

  import GeneticSystem.{Gene, Chromosome, Genome, Global, chordToPitches, pitchesToChord, varToPitch, pitchToVar}

  def apply(gen: Genome, num: Int, glob: Global, r: util.Random): Genome = Vec.fill(num) {
    val i   = r.nextInt(gen.size)
    val gi  = gen(i)
    val mut = numGenes(gi)(r)

    (gi /: (1 to mut)) { (gj, _) =>
      var tries = 1000
      var res   = gj
      while (tries > 0) {
        val j     = r.nextInt(gj.size)
        val in    = gj(j)
        if (in._2 == ChordGood) {
          tries -= 1
        } else {
          // val pred  = if (j > 0          ) Some(gj(j - 1)) else None
          // val succ  = if (j < gj.size - 1) Some(gj(j + 1)) else None
          val m     = mutate(glob, gj, j)(r)
          res       = gj.updated(j, m)
          tries     = 0
        }
      }
      res
    }
  }

  /** The number of genes to mutate, given a particular chromosome. Defaults to
    * random number between `chordMin` and `chordMax` applied with the chromosome size.
    */
  protected def numGenes(chromosome: Chromosome)(implicit random: util.Random): Int = {
    val sz  = chromosome.size
    val min = chordMin(sz)
    val max = chordMax(sz)
    rangeRand(min, max)
  }

  protected def numVoices(global: Global)(implicit random: util.Random): Int = {
    val sz  = global.voices.size
    val min = voiceMin(sz)
    val max = voiceMax(sz)
    rangeRand(min, max)
  }

  def mutate(global: Global, gj: Chromosome, j: Int)(implicit r: util.Random): Gene = {
    // val pred  = if (j > 0          ) Some(gj(j - 1)) else None
    // val succ  = if (j < gj.size - 1) Some(gj(j + 1)) else None
    val gene  = gj(j)

    val (chord, _) = gene
    val csz   = chord.size
    require(csz == global.voices.size)

    val num   = numVoices(global)
    val sel   = ((0 until csz) diff excludeVoiceNums).scramble().take(num).toSet

    import poirot.{Vec => _, _}
    implicit val model = Model()
    import Implicits._

    if (DEBUG_MUTA) println()

    // val varsX = Vec.fill(gj.size)(Vec.fill(csz)(IntVar(Int.MinValue, Int.MaxValue)))
    val varsX = gj.zipWithIndex.map { case ((_chord, _), idx) =>
      val pitchesSel = chordToPitches[Pitch](_chord).zipWithIndex // .zip(sel)
      val _vars = pitchesSel.map { case (p, pSel) =>
        val midi = p.midi
        if (idx == j && sel.contains(pSel)) {
          val lo = midi - interval
          val hi = midi + interval
          IntVar(lo, hi)

        } else
          IntVar(midi, midi)
      }

      // vertical constraints
      GeneticSystem.constrainVertical(_vars, global.voices, global.vertical)
      _vars
    }

    // horizontal constraints
    varsX.mapPairs { (c1, c2) =>
      GeneticSystem.constrainHoriz(c1, c2, global.voices)
    }

    global.global.foreach(_.apply(varsX))

    require(model.consistency(), s"Constraints model is not consistent")

    val vars = varsX(j)

    val select        = search(vars, firstFail, indomainRandom(r))
    val solutionsB    = Vec.newBuilder[Chord]
    maxNumSolutions   = 256
    timeOut           = 10    // seconds

    def addSolution(): Unit = solutionsB += pitchesToChord(vars)

    val result        = satisfyAll(select, addSolution)
    val solutions     = solutionsB.result()

    if (!result) {
      //      val thisInfo = pitchesSel.reverse.map { case (p, s) => s"$p${if (sel contains s) "!" else ""}" } .mkString(", ")
      //      val predInfo = pred.fold("") { case (c, _) =>
      //        c.pitches.mkString("; pred = ", ", ", "")
      //      }
      //      val succInfo = succ.fold("") { case (c, _) =>
      //        c.pitches.mkString("; succ = ", ", ", "")
      //      }
      val thisInfo  = gj(j)
      val predInfo  = if (j <= 0) "" else {
        val (c, _) = gj(j - 1)
        c.pitches.mkString("; pred = ", ", ", "")
      }
      val succInfo  = if (j >= gj.size) "" else {
        val (c, _) = gj(j + 1)
        c.pitches.mkString("; succ = ", ", ", "")
      }

      if (WARN_MUTA) println(s"Warning: could not mutate $thisInfo$predInfo$succInfo")
      return gene
    }

    val chordOut = solutions.choose()
    if (DEBUG_MUTA) println(s"Mutated: ${chordOut.pitches.mkString(", ")}")
    chordOut -> ChordNeutral
  }

  //  def mutateOLD(gene0: Gene, pred: Option[Gene], succ: Option[Gene])(implicit r: util.Random): Gene = {
  //    val pitches   = gene.pitches.scramble
  //    val (a, b)    = pitches.splitAt(num)
  //    val bMid      = b.map(_.midi)
  //
  //    @tailrec def mkMut(): Vec[Pitch] = {
  //      val mut       = a.map { p =>
  //        val old     = p.midi
  //        val poss    = (old - interval to old + interval) diff (bMid :+ old)
  //
  //        val nu = if (poss.isEmpty) old else {
  //          val i     = r.nextInt(poss.size)
  //          poss(i)
  //        }
  //        nu.asPitch
  //      }
  //      if (mut == mut.distinct) mut else mkMut() // cheesy way to repeat if there are duplicates
  //    }
  //
  //    val newP    = mkMut() ++ b
  //    val notes   = (gene.notes zip newP).map {
  //      case (n, p) => n.copy(pitch = p)
  //    }
  //
  //    val gene1 = gene.copy(notes = notes.sortBy(_.pitch))
  //    (gene1, if (gene1 == gene) gene0._2 else ChordNeutral)
  //  }
}

/** Constraint on the vertical (harmonic) structure. */
sealed trait VerticalConstraint {
  // /** Verifies whether the constraint is satisfied (`true`) or not (`false`). */
  // def apply(chord: Chord): Boolean

  def apply(chord: Vec[IntVar])(implicit m: poirot.Model): Unit
}

//sealed trait ForbiddenIntervalLike {
//  /** Forbidden intervals sorted top to bottom */
//  protected def intervals: Vec[Int]
//
//  def apply(chord: Vec[IntVar])(implicit m: poirot.Model): Unit = {
//    import poirot.Implicits._
//
//    val ivalsForbidden  = intervals
//    val sz              = ivalsForbidden.size
//    chord.combinations(sz + 1).foreach { case sub =>
//      // sub is the filtered chord, having `sz + 1` pitches from hi to lo.
//      // we calculate the `sz` relative intervals of these pitches.
//      // then the modulus of these intervals with the forbidden intervals.
//      // we forbid that all modulus are zero, which is written as
//      // the sum of the modulus not being zero. (see `TwoIntervalTest.scala`).
//
//      val ivalsFound  = sub.pairMap(_ - _)
//      val mod         = (ivalsFound zip ivalsForbidden).map { case (a, b) => a % b }
//      val sum         = mod.reduce(_ + _)
//      sum #!= 0
//    }
//  }
//}

sealed trait ForbiddenIntervalLike {
  /** Forbidden intervals sorted top to bottom */
  protected def intervals: Vec[Int]

  def apply(chord: Vec[IntVar])(implicit m: poirot.Model): Unit = {
    import poirot.Implicits._

    val ivalsForbidden  = intervals
    val sz              = ivalsForbidden.size
    chord.combinations(sz + 1).foreach { case sub =>
      // sub is the filtered chord, having `sz + 1` pitches from hi to lo.
      // we calculate the `sz` relative intervals of these pitches.
      // then the modulus of these intervals with the forbidden intervals.
      // we forbid that all modulus are zero, which is written as
      // the sum of the modulus not being zero. (see `TwoIntervalTest.scala`).

      val ivalsFound  = sub.mapPairs(_ - _)
      val mod         = (ivalsFound zip ivalsForbidden).map { case (a, b) => a % b }
      val sum         = mod.reduce(_ + _)
      sum #!= 0
    }
  }
}

/** Constraint which forbids to occurrence of a particular interval */
case class ForbiddenInterval(steps: Int = 12) extends VerticalConstraint with ForbiddenIntervalLike {
  protected val intervals = Vec(steps)
}

/** Constraint which forbids to co-presence of two given intervals.
  *
  * @param top      the top (upper) interval
  * @param bottom   the bottom (lower) interval
  */
case class ForbiddenIntervalPair(top: Int = 3, bottom: Int = 2) extends VerticalConstraint with ForbiddenIntervalLike {
  protected val intervals = Vec(top, bottom)
}

case class ForbiddenIntervalTriple(top: Int = 4, mid: Int = 3, bottom: Int = 2)
  extends VerticalConstraint with ForbiddenIntervalLike {

  protected val intervals = Vec(top, mid, bottom)
}

sealed trait EvaluationImpl extends muta.Evaluation[GeneticSystem.Chromosome]

case class ParallelEval(funs: Vec[EvaluationImpl], aggr: ReduceFunction = Mean) extends EvaluationImpl {
  override def apply(c: GeneticSystem.Chromosome): Double = aggr(funs.map(_.apply(c)))
}

case class FrameIntervalEval(reduce: ReduceFunction =
                             Match(gen = ConstantEnv(13), AbsDif, ComposeReduceUnary(Mean, LinLin(0, 10, 1, 0))))
  extends EvaluationImpl {

  override def apply(c: GeneticSystem.Chromosome): Double = {
    val dSeq = c.map(_._1.frameInterval.semitones.toDouble)
    reduce(dSeq)
  }
}

case class VoiceEval(voice: Int = 1, reduce: ReduceFunction =
                    Match(gen = ConstantEnv(60), AbsDif, ComposeReduceUnary(Mean, LinLin(0, 10, 1, 0))))
  extends EvaluationImpl {

  import GeneticSystem.{Chromosome, chordToPitches}

  override def apply(c: Chromosome): Double = {
    val seq = c.map { case (chord, _) => chordToPitches[Double](chord)(_.midi.toDouble)(voice - 1) }
    reduce(seq)
  }
}

object EvaluationImpl {
  implicit object format extends Format[EvaluationImpl] {
    def reads(json: JsValue): JsResult[EvaluationImpl] = ???

    def writes(eval: EvaluationImpl): JsValue = ???
  }
}

sealed trait EnvelopeFunction extends (Int => Vec[Double])

case class ConstantEnv(value: Double = 0.0) extends EnvelopeFunction {
  override def apply(num: Int): Vec[Double] = Vec.fill(num)(value)
}

case class ScaleEnv(scale: UnaryOp = LinearEnvelope(0, 1)) extends EnvelopeFunction {
  override def apply(num: Int): Vec[Double] = Vec.tabulate(num)(i => scale(i.toDouble / (num - 1)))
}

sealed trait ReduceFunction extends (Vec[Double] => Double)

case class Match(gen: EnvelopeFunction = ConstantEnv(), op: BinaryOp = AbsDif, aggr: ReduceFunction = Mean)
  extends ReduceFunction {

  override def apply(x: Vec[Double]): Double = {
    val y = gen(x.size)
    val d = (x zip y).map(op.tupled)
    aggr(d)
  }
}

case object Mean extends ReduceFunction {
  def apply(fits: Vec[Double]): Double = fits.sum / fits.size
}

case object RootMeanSquare extends ReduceFunction {
  def apply(fits: Vec[Double]): Double = 1.0 / math.sqrt(fits.map(x => 1.0/(x * x)).sum / fits.size)
}

case object Minimum extends ReduceFunction {
  def apply(fits: Vec[Double]): Double = fits.min
}

case object Maximum extends ReduceFunction {
  def apply(fits: Vec[Double]): Double = fits.max
}

case class ReduceWeighted(balance: Double = 0.5, fun: ReduceFunction = Mean) extends ReduceFunction {
  def apply(fits: Vec[Double]): Double = {
    val sz  = fits.size
    val w   = Vec.tabulate(sz) { i =>
      import numbers.Implicits._
      i.linlin(0, sz - 1, 1 - balance, balance)
    }
    val m = (fits zip w).map { case (f, i) => f * i }
    fun(m)
  }
}

case class ComposeReduceUnary(first: ReduceFunction, andThen: UnaryOp) extends ReduceFunction {
  override def apply(in: Vec[Double]): Double = andThen(first(in))
}

case class ComposeUnaryReduce(first: UnaryOp, andThen: ReduceFunction) extends ReduceFunction {
  override def apply(in: Vec[Double]): Double = andThen(in.map(first.apply))
}

object ReduceFunction {
  implicit object format extends Format[ReduceFunction] {
    def reads(json: JsValue): JsResult[ReduceFunction] = ???

    def writes(fun: ReduceFunction): JsValue = ???
  }
}

object ManualGeneticSystem extends GeneticSystem {
  def manual = true
}
object GeneticSystem {
  type Global     = GlobalImpl
  type Gene       = (Chord, ChordEval)
  type Chromosome = Vec[Gene]
  type Generation = GenerationImpl
  type Evaluation = EvaluationImpl
  type Selection  = SelectionImpl
  type Breeding   = BreedingImpl

  type Genome     = Vec[Chromosome]

  def chordToPitches[A](chord: Chord)(implicit view: Pitch => A): Vec[A] = chord.pitches.reverse.map(view)
  def pitchesToChord[A](pitches: Vec[A])(implicit view: A => Pitch): Chord = {
    val notes = pitches.reverse.map(pitch => OffsetNote(offset = 0, pitch = view(pitch), duration = 1, velocity = 80))
    Chord(notes)
  }

  def accept(c: Chromosome, global: Global): Boolean = {
    import poirot._
    implicit val model = Model()
    // import Implicits._

    val vars = c.map { case (chord, _) => chordToPitches[IntVar](chord) }
    vars.foreach (constrainVertical (_   , global.voices, global.vertical))
    vars.mapPairs(constrainHoriz    (_, _, global.voices))
    global.global.foreach(_.apply(vars))

    val select = search(vars.flatten, smallest, indomainMin)
    satisfy(select)
  }

  def constrainVertical(cv: Vec[IntVar], voices: Vec[Voice],
                        vertical: Vec[VerticalConstraint])
                       (implicit m: poirot.Model): Unit = {
    import poirot._
    // voice registers
    (cv zip voices).foreach { case (v, vc) =>
      v #>= vc.down.limit
      v #<= vc.up  .limit
    }
    // no voices crossing
    cv.mapPairs { case (hi, lo) =>
      hi #> lo
    }
    // custom vertical constraints
    vertical.foreach(_.apply(cv))
  }

  def stringToIntervals(s: String): Vec[Int] = {
    val t = s.trim
    if (t.isEmpty) Vec.empty
    else t.split(' ').collect {
      case x if !x.isEmpty => x.toInt
    } (breakOut)
  }

  def constrainHoriz(pred: Vec[IntVar], succ: Vec[IntVar],
                     voices: Vec[Voice])(implicit m: poirot.Model): Unit = {
    voices.zipWithIndex.foreach { case (vc, vci) =>
      val p1 = pred(vci)
      val p2 = succ(vci)
      p2 #<= (p1 + vc.up  .step)
      p2 #>= (p1 - vc.down.step)

      ((vc.up, 1) :: (vc.down, -1) :: Nil).foreach { case (dir, sign) =>
        val ivals = stringToIntervals(dir.intervals)
        val forb  = dir.constraint match {
          case Forbid => ivals
          case Allow  => (0 to dir.step).diff(ivals)   // invert: forbid all but the mentioned intervals
        }
        if (forb.nonEmpty) {
          val stepFound       = p2 - p1
          val stepsForbidden  = forb.map(_ * sign)
          stepsForbidden.foreach { stepForbidden =>
            stepFound #!= stepForbidden
          }
        }
      }
    }
  }

  val DefaultVoices = Vec(
    Voice(down = VoiceDirection(limit = 48, step = 6), up = VoiceDirection(limit = 96, step = 6)),
    Voice(down = VoiceDirection(limit = 36, step = 6), up = VoiceDirection(limit = 84, step = 6)),
    Voice(down = VoiceDirection(limit = 24, step = 6), up = VoiceDirection(limit = 72, step = 6))
  )

  implicit def varToPitch(vr: IntVar): Pitch = vr.value().asPitch
  implicit def pitchToVar(pitch: Pitch)(implicit model: poirot.Model): IntVar = IntVar(pitch.midi, pitch.midi)
}
trait GeneticSystem extends muta.System {
  def manual: Boolean

  type Global     = GeneticSystem.Global
  type Gene       = GeneticSystem.Gene
  type Chromosome = GeneticSystem.Chromosome
  type Generation = GeneticSystem.Generation
  type Evaluation = GeneticSystem.Evaluation
  type Selection  = GeneticSystem.Selection
  type Breeding   = GeneticSystem.Breeding

  def defaultGeneration: Generation = GenerationImpl()
  def defaultEvaluation: Evaluation = FrameIntervalEval()
  def defaultSelection : Selection  = SelectionRoulette()
  def defaultBreeding  : Breeding   = BreedingImpl()

  val chromosomeClassTag = reflect.classTag[Chromosome]

  def generationView(init: Generation, config: Config) = AutoView(init, config)
  def evaluationView(init: Evaluation, config: Config) = AutoView(init, config)
  def selectionView (init: Selection , config: Config) = AutoView(init, config)
  def breedingView  (init: Breeding  , config: Config) = AutoView(init, config)

  // Option[(Evaluation, AutoView.Config) => AutoView[Evaluation]]
  def evaluationViewOption = if (manual) None else Some(evaluationView _)

  private val chordSeqView  = new ChordSeqView
  private val chordSeqView2 = new ChordSeqView
  // chordSeqView2.border = Swing.EmptyBorder(1)

  override def chromosomeView(c: Chromosome, default: Label, selected: Boolean, focused: Boolean) = {
    // default.text = c.map(_.pitches.mkString("[", " ", "]")).mkString(" ")
    // default
    if (c != null) chordSeqView.chords = c
    chordSeqView
  }

  // human evaluation
  override def humanEvaluationSteps = if (manual) 6 else 0

  private def setChromoEditor(c: Chromosome): Unit =
    if (c != null) {
      chordSeqView2.chords = c
    }

  override def chromosomeEditorOption: Option[(Component, () => Chromosome, (Chromosome) => Unit)] =
    Some((chordSeqView2, () => chordSeqView2.chords, setChromoEditor))

  // serialization
  private implicit val fivalformat  = AutoFormat[ForbiddenInterval]
  private implicit val vconsFormat  = AutoFormat[VerticalConstraint]
  // private implicit val voiceFormat  = AutoFormat[Voice]
  private implicit val globalFormat = AutoFormat[GlobalImpl]
  // private implicit val alleleFormat = AutoFormat[(Chord, ChordEval)]

  private implicit val geneFormat   = new Format[Gene] {
    def reads(json: JsValue): JsResult[Gene] = json match {
      case JsObject(sq1) =>
        val m     = sq1.toMap
        val cj    = m.getOrElse("chord", sys.error(s"Field 'chord' not found in $sq1"))
        val chord = ChordFormat.reads(cj).get
        val eval  = m.get("eval") match {
          case Some(JsString("good")) => ChordGood
          case Some(JsString("bad" )) => ChordBad
          case None                   => ChordNeutral
          case other                  => sys.error(s"Unexpected 'eval' value $other")
        }
        JsSuccess((chord, eval))

      case other => JsError(s"Not a JSON object $other")
    }

    def writes(gene: Gene): JsValue = {
      val (chord, eval) = gene
      val fields0 = ("chord" -> ChordFormat.writes(chord)) :: Nil
      val fields  = if (eval == ChordNeutral) fields0 else {
        ("eval" -> JsString(eval match {
          case ChordGood    => "good"
          case ChordBad     => "bad"
          case ChordNeutral => assert(assertion = false); ""
        })) :: fields0
      }
      JsObject(fields)
    }
  }
  val chromosomeFormat  = AutoFormat[Vec[Gene]]

  val generationFormat  = AutoFormat[GenerationImpl]
  val selectionFormat   = AutoFormat[SelectionImpl ]
  private implicit val breedingFunFormat  = AutoFormat[BreedingFunctionImpl]
  val breedingFormat    = AutoFormat[BreedingImpl  ]
  // private implicit val unaryOpFormat    = AutoFormat[UnaryOp]
  // private implicit val binaryOpFormat   = AutoFormat[BinaryOp]
  // private implicit val reduceFunFormat  = AutoFormat[ReduceFunction]
  val evaluationFormat  = EvaluationImpl.format // AutoFormat[EvaluationImpl]
}