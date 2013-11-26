package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import at.iem.point.illism._
import scala.util.Random
import de.sciss.muta.{SelectionNumber, SelectionPercent, SelectionSize}
import scala.swing.{Component, Label}
import play.api.libs.json.{JsSuccess, JsError, JsString, JsObject, JsResult, JsValue, Format}
import de.sciss.jacop
import JaCoP.search.{SmallestDomain, SimpleSelect}
import de.sciss.numbers
import de.sciss.play.json.AutoFormat

case class Voice(maxUp: Int = 2, maxDown: Int = 2, lowest: Int = 36, highest: Int = 96)
object Voice {
  implicit val format = AutoFormat[Voice]
}

/** The global configuration for the generation of chromosomes.
  *
  * @param voices    the number of voices (vertical chord size)
  * @param vertical  vertical constraints
  * @param length    the number of chords to produce
  */
case class GlobalImpl(voices  : Vec[Voice] = GeneticSystem.DefaultVoices,
                      vertical: Vec[VerticalConstraint] = Vec(ForbiddenInterval(12)),
                      length  : Int = 12)

case class GenerationImpl(size: Int = 100, global: GlobalImpl = GlobalImpl(), seed: Int = 0)
  extends muta.Generation[GeneticSystem.Chromosome, GlobalImpl] {

  def apply(r: Random): GeneticSystem.Chromosome = {
    val voices    = global.voices
    val numVoices = voices.size
    val num       = global.length

    import jacop._
    implicit val model = new Model
    import Implicits._

    val vars = Vec.fill(num) {
      val ch = Vec.fill(numVoices)(new IntVar())
      GeneticSystem.constrainVert(ch, voices, global.vertical)
      ch
    }
    vars.foreachPair { (c1, c2) =>
      GeneticSystem.constrainHoriz(c1, c2, voices)
    }

    require(model.consistency(), s"Constraints model is not consistent")

    def mkChromo(): GeneticSystem.Chromosome = vars.map { vc =>
      val pitches = vc.map(v => v.value().asPitch).reverse
      val notes   = pitches.map(pitch => OffsetNote(offset = 0, pitch = pitch, duration = 1, velocity = 80))
      (Chord(notes), ChordNeutral)
    }

    val select        = new SimpleSelect[IntVar](vars.flatten.toArray,
      new SmallestDomain[IntVar](),
      new IndomainRandom2(r) // new IndomainMin[IntVar]()
    )
    val solutionsB    = Vec.newBuilder[GeneticSystem.Chromosome]
    limitOnSolutions  = math.max(1, math.min(math.pow(size, 1.5), 16384).toInt)
    timeOutValue      = 30    // seconds
    val result        = satisfyAll[IntVar](select, () => solutionsB += mkChromo())
    val solutions     = solutionsB.result()

    require(result, s"Constraints could not be satisfied")
    // println(s"num-solutions ${solutions.size}")

    solutions.choose(r)
  }
}

sealed trait SelectionImpl extends muta.Selection[GeneticSystem.Chromosome]

case class SelectionRoulette(size: SelectionSize = SelectionPercent(33))
  extends muta.impl.SelectionRouletteImpl[GeneticSystem.Chromosome] with SelectionImpl

case class SelectionTruncation(size: SelectionSize = SelectionPercent(20))
  extends muta.impl.SelectionTruncationImpl[GeneticSystem.Chromosome] with SelectionImpl

sealed trait BreedingFunctionImpl extends muta.BreedingFunction[GeneticSystem.Chromosome, GlobalImpl]

case class BreedingImpl(elitism       : SelectionSize     = SelectionNumber(5),
                        crossoverWeight: SelectionPercent  = SelectionPercent(0 /* 50 */),  // crossover doesn't honor constraints yet
                        crossover      : BreedingFunctionImpl  = Crossover,
                        mutation       : BreedingFunctionImpl  = Mutation())
  extends muta.impl.BreedingImpl[GeneticSystem.Chromosome, GlobalImpl]

object Crossover extends muta.impl.CrossoverVecImpl[GeneticSystem.Gene, GlobalImpl] with BreedingFunctionImpl

case class Mutation(chords: SelectionSize = SelectionPercent(20),
                    voices: SelectionSize = SelectionNumber(3), interval: Int = 7)
  extends BreedingFunctionImpl {

  def numGenesSize = chords

  import GeneticSystem.{Gene, Chromosome, Genome, Global}

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
          val pred  = if (j > 0          ) Some(gj(j - 1)) else None
          val succ  = if (j < gj.size - 1) Some(gj(j + 1)) else None
          val m     = mutate(glob, in, pred, succ)(r)
          res       = gj.updated(j, m)
          tries     = 0
        }
      }
      res
    }
  }

  /** The number of genes to mutate, given a particular chromosome. Defaults to
    * applying `numGenesSize` with the chromosome size.
    */
  protected def numGenes(chromosome: Chromosome)(implicit random: util.Random): Int =
    random.nextInt(numGenesSize(chromosome.size))

  def mutate(global: Global, gene: Gene, pred: Option[Gene], succ: Option[Gene])(implicit r: util.Random): Gene = {
    val (chord, _) = gene
    val csz   = chord.size
    require(csz == global.voices.size)

    val num   = voices(csz)
    val sel   = Vec.tabulate(csz)(_ < num).scramble

    import jacop._
    implicit val model = new Model
    import Implicits._

    if (DEBUG_MUTA) println()

    val pitchesSel = chord.pitches.reverse.zip(sel)
    val vars = pitchesSel.map { case (p, psel) =>
      val midi = p.midi
      if (psel) {
        val lo = midi - interval
        val hi = midi + interval
        new IntVar(lo, hi)
      } else new IntVar(midi, midi)
    }
    GeneticSystem.constrainVert(vars, global.voices, global.vertical)
    //    vars.foreachPair { case (hi, lo) =>
    //      if (DEBUG_MUTA) println(s"{${hi.min()}...${hi.max()}} #> {${lo.min()}...${lo.max()}}")
    //      hi #> lo
    //    }

    pred.foreach { case (predC, _) =>
      val pred: Vec[IntVar] = predC.pitches.reverse.map(_.midi: IntVar)
      GeneticSystem.constrainHoriz(pred, vars, global.voices)
      //      vars.zip(predC.pitches.reverse).zip(glob.voices).foreach { case ((iv, predP), vc) =>
      //        val pp = predP.midi
      //        if (DEBUG_MUTA) println(s"Pred: ${pp - vc.maxDown} #<= {${iv.min()}...${iv.max()}} #<= ${pp + vc.maxUp}")
      //        iv #<= (pp + vc.maxUp  )
      //        iv #>= (pp - vc.maxDown)
      //      }
    }
    // something with the following is wrong probably:
    succ.foreach { case (succC, _) =>
      val succ: Vec[IntVar] = succC.pitches.reverse.map(_.midi: IntVar)
      GeneticSystem.constrainHoriz(vars, succ, global.voices)
      //      vars.zip(succC.pitches.reverse).zip(global.voices).foreach { case ((iv, succP), vc) =>
      //        val sp = succP.midi
      //        if (DEBUG_MUTA) println(s"Succ: ${sp + vc.maxDown} #<= {${iv.min()}...${iv.max()}} #<= ${sp - vc.maxUp}")
      //        iv #>= (sp - vc.maxUp  )
      //        iv #<= (sp + vc.maxDown)
      //      }
    }

    require(model.consistency(), s"Constraints model is not consistent")

    def mkChord(): Chord = {
      val pitches = vars.map(v => v.value().asPitch).reverse
      val notes   = pitches.map(pitch => OffsetNote(offset = 0, pitch = pitch, duration = 1, velocity = 80))
      Chord(notes)
    }

    val select        = new SimpleSelect[IntVar](vars.toArray,
      new SmallestDomain[IntVar](),
      new IndomainRandom2(r) // new IndomainMin[IntVar]()
    )
    val solutionsB    = Vec.newBuilder[Chord]
    limitOnSolutions  = 256
    timeOutValue      = 10    // seconds
    val result        = satisfyAll[IntVar](select, () => solutionsB += mkChord())
    val solutions     = solutionsB.result()

    if (!result) {
      val thisInfo = pitchesSel.reverse.map { case (p, s) => s"$p${if (s) "!" else ""}" } .mkString(", ")
      val predInfo = pred.fold("") { case (c, _) =>
        c.pitches.mkString("; pred = ", ", ", "")
      }
      val succInfo = succ.fold("") { case (c, _) =>
        c.pitches.mkString("; succ = ", ", ", "")
      }
      if (WARN_MUTA) println(s"Warning: could not mutate $thisInfo$predInfo$succInfo")
      return gene
    }

    val chordOut = solutions.choose(r)
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
  // /** Verifies whether the contraint is satisfied (`true`) or not (`false`). */
  // def apply(chord: Chord): Boolean

  def apply(chord: Vec[jacop.IntVar])(implicit m: jacop.Model): Unit
}
/** Constraint which forbids to occurrence of a particular interval */
case class ForbiddenInterval(steps: Int = 12) extends VerticalConstraint {
  //  def apply(chord: Chord): Boolean = chord.allIntervals.forall { ival =>
  //    val i0  = ival.semitones
  //    val i   = if (i0 <= 12) i0 else (i0 - 12) % 12 + 12 // preserve octave!!
  //    i != steps
  //  }

  def apply(chord: Vec[jacop.IntVar])(implicit m: jacop.Model): Unit = {
    import jacop._
    import Implicits._
    chord.combinations(2).foreach { case Vec(hi, lo) =>
      // note: we assume hi > lo, so unison cannot be mixed up with octave
      (hi - lo).mod(steps) #\= 0
    }
  }
}

/** Constraint which forbids to co-presence of two given intervals.
  *
  * @param a          the first interval
  * @param b          the second interval
  * @param neighbor   if `true`, intervals `a` and `b` may not occur as neighboring intervals, if `false`
  *                   then `a` and `b` may not occur anywhere in the chord.
  */
case class ForbiddenIntervalPair(a: ForbiddenInterval = ForbiddenInterval(6),
                                 b: ForbiddenInterval = ForbiddenInterval(6), neighbor: Boolean = false)
  extends VerticalConstraint {

  def apply(chord: Vec[jacop.IntVar])(implicit m: jacop.Model): Unit = {
    import jacop._
    import Implicits._

    val modA = new jacop.IntVar()
    val modB = new jacop.IntVar()

    chord.combinations(2).foreach { case Vec(hi, lo) =>
      (hi - lo).mod(a.steps) #\= modA
    }
    chord.combinations(2).foreach { case Vec(hi, lo) =>
      (hi - lo).mod(b.steps) #\= modB
    }

    // modA #= 0 <=> bA
    // modB #= 0 <=> bB

    ???
  }
}

sealed trait EvaluationImpl extends muta.Evaluation[GeneticSystem.Chromosome]

case class ParallelEval(funs: Vec[EvaluationImpl], aggr: ReduceFunction = Mean) extends EvaluationImpl {
  override def apply(c: GeneticSystem.Chromosome): Double = aggr(funs.map(_.apply(c)))
}

case class FrameIntervalEval(reduce: ReduceFunction =
                             Match(gen = ConstantEnv(13), AbsDif, ComposeReduceUnary(Mean, LinLin(0, 10, 1, 0))))
  extends EvaluationImpl {

  override def apply(c: GeneticSystem.Chromosome): Double = {
    val dseq = c.map(_._1.frameInterval.semitones.toDouble)
    reduce(dseq)
  }
}

case class VoiceEval(voice: Int = 1, reduce: ReduceFunction =
                    Match(gen = ConstantEnv(60), AbsDif, ComposeReduceUnary(Mean, LinLin(0, 10, 1, 0))))
  extends EvaluationImpl {

  override def apply(c: GeneticSystem.Chromosome): Double = {
    val seq = c.map(_._1.pitches.reverse(voice).midi.toDouble)
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

object GeneticSystem extends muta.System {
  def manual = true

  val DefaultVoices = Vec(
//    Voice(lowest = 72, highest = 96),
//    Voice(lowest = 48, highest = 72),
//    Voice(lowest = 24, highest = 48)
    Voice(lowest = 48, highest = 96, maxUp = 6, maxDown = 6),
    Voice(lowest = 36, highest = 84, maxUp = 6, maxDown = 6),
    Voice(lowest = 24, highest = 72, maxUp = 6, maxDown = 6)
  )

  type Global     = GlobalImpl
  type Gene       = (Chord, ChordEval)
  type Chromosome = Vec[Gene]
  type Generation = GenerationImpl
  type Evaluation = EvaluationImpl
  type Selection  = SelectionImpl
  type Breeding   = BreedingImpl

  def constrainVert(cv: Vec[jacop.IntVar], voices: Vec[Voice],
                     vertical: Vec[VerticalConstraint])
                    (implicit m: jacop.Model): Unit = {
    import jacop._
    // voice registers
    (cv zip voices).foreach { case (v, vc) =>
      v #>= vc.lowest
      v #<= vc.highest
    }
    // no voices crossing
    cv.foreachPair { case (hi, lo) =>
      hi #> lo
    }
    // custom vertical constraints
    vertical.foreach(_.apply(cv))
  }

  def constrainHoriz(pred: Vec[jacop.IntVar], succ: Vec[jacop.IntVar],
                     voices: Vec[Voice])(implicit m: jacop.Model): Unit = {
    voices.zipWithIndex.foreach { case (vc, vci) =>
      val p1 = pred(vci)
      val p2 = succ(vci)
      p2 #<= (p1 + vc.maxUp  )
      p2 #>= (p1 - vc.maxDown)
    }
  }

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
  def evaluationViewOption = if (manual) None else Some(evaluationView)

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