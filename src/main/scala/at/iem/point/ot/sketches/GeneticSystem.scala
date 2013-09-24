package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import at.iem.point.illism._
import scala.util.{Success, Failure, Try, Random}
import de.sciss.muta.{SelectionNumber, SelectionPercent, SelectionSize}
import scala.swing.{Component, Label}
import scala.annotation.tailrec
import play.api.libs.json.{JsSuccess, JsError, JsString, JsObject, JsResult, JsArray, JsValue, Format, SealedTraitFormat}
import collection.breakOut
import de.sciss.jacop
import JaCoP.search.{IndomainRandom, SmallestDomain, IndomainMin, SimpleSelect}

case class Voice(maxUp: Int = 2, maxDown: Int = 2, lowest: Int = 36, highest: Int = 96)

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
      val cv = voices.map { vc =>
        new IntVar(vc.lowest, vc.highest)
      }
      cv.foreachPair { case (hi, lo) =>
        hi #> lo
      }
      cv
    }
    vars.foreachPair { (c1, c2) =>
      voices.zipWithIndex.foreach { case (vc, vci) =>
        val p1 = c1(vci)
        val p2 = c2(vci)
        p2 #<= (p1 + vc.maxUp  )
        p2 #>= (p1 - vc.maxDown)
      }
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

sealed trait EvaluationImpl extends muta.Evaluation[GeneticSystem.Chromosome]

sealed trait SelectionImpl extends muta.Selection[GeneticSystem.Chromosome]

case class SelectionRoulette(size: SelectionSize = SelectionPercent(33))
  extends muta.impl.SelectionRouletteImpl[GeneticSystem.Chromosome] with SelectionImpl

case class SelectionTruncation(size: SelectionSize = SelectionPercent(20))
  extends muta.impl.SelectionTruncationImpl[GeneticSystem.Chromosome] with SelectionImpl

sealed trait BreedingFunctionImpl extends muta.BreedingFunction[GeneticSystem.Chromosome, GlobalImpl]

case class BreedingImpl(elitism       : SelectionSize     = SelectionNumber(20),
                        crossoverWeight: SelectionPercent  = SelectionPercent(50),
                        crossover      : BreedingFunctionImpl  = Crossover,
                        mutation       : BreedingFunctionImpl  = Mutation())
  extends muta.impl.BreedingImpl[GeneticSystem.Chromosome, GlobalImpl]

object Crossover extends muta.impl.CrossoverVecImpl[(Chord, ChordEval), GlobalImpl] with BreedingFunctionImpl

case class Mutation(chords: SelectionSize = SelectionPercent(20),
                    voices: SelectionSize = SelectionNumber(3), interval: Int = 7)
  extends muta.impl.MutationVecImpl [(Chord, ChordEval), GlobalImpl] with BreedingFunctionImpl {

  override protected val numGenesSize = chords

  def mutate(gene0: (Chord, ChordEval))(implicit r: util.Random): (Chord, ChordEval) = {
    // println("Muta!")
    val gene = gene0._1

    val num       = r.nextInt(math.max(0, voices(gene.size) - 1) + 1)
    val pitches   = gene.pitches.scramble
    val (a, b)    = pitches.splitAt(num)
    val bMid      = b.map(_.midi)

    @tailrec def mkMut(): Vec[Pitch] = {
      val mut       = a.map { p =>
        val old     = p.midi
        val poss    = (old - interval to old + interval) diff (bMid :+ old)

        val nu = if (poss.isEmpty) old else {
          val i     = r.nextInt(poss.size)
          poss(i)
        }
        nu.asPitch
      }
      if (mut == mut.distinct) mut else mkMut() // cheesy way to repeat if there are duplicates
    }

    val newP    = mkMut() ++ b
    val notes   = (gene.notes zip newP).map {
      case (n, p) => n.copy(pitch = p)
    }

    val gene1 = gene.copy(notes = notes.sortBy(_.pitch))
    (gene1, if (gene1 == gene) gene0._2 else ChordNeutral)
  }
}

/** Constraint on the vertical (harmonic) structure. */
sealed trait VerticalConstraint {
  /** Verifies whether the contraint is satisfied (`true`) or not (`false`). */
  def apply(chord: Chord): Boolean
}
/** Constraint which forbids to occurrence of a particular interval */
case class ForbiddenInterval(steps: Int = 12) extends VerticalConstraint {
  def apply(chord: Chord): Boolean = chord.allIntervals.forall { ival =>
    val i0  = ival.semitones
    val i   = if (i0 <= 12) i0 else (i0 - 12) % 12 + 12 // preserve octave!!
    i != steps
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

  def apply(chord: Chord): Boolean = ???
}

case class SampleEvaluation(highest: Int = 96, lowest: Int = 36, maxUp: Int = 2, maxDown: Int = 2) extends EvaluationImpl {
  def evalLine(line: Vec[Pitch]): Double = {
    val midi      = line.map(_.midi)
    // val numReg    = midi.count(m => m >= lowest && m <= highest)
    val numReg    = midi.map { m =>
      if (m < lowest)
        m.toDouble / lowest
      else if (m > highest)
        highest.toDouble / m
      else
        1.0
    } .sum

    val steps     = midi.pairDiff
    // val numIval   = steps.count { m =>
    //   m <= maxUp && -m <= maxDown
    // }
    val numIval   = steps.map { m =>
      if (m < -maxDown)
        maxDown.toDouble / -m
      else if (m > maxUp)
        maxUp.toDouble / m
      else
        1.0
    } .sum
    val numGood   = numReg + numIval
    val maxGood   = midi.size + steps.size
    numGood / maxGood
  }

  def apply(c: GeneticSystem.Chromosome): Double = {
    val lines = Horizontal.fromChords(c.map(_._1))
    lines.map(evalLine).mean
  }
}

object GeneticSystem extends muta.System {
  val DefaultVoices = Vec(
    Voice(lowest = 72, highest = 96),
    Voice(lowest = 48, highest = 72),
    Voice(lowest = 24, highest = 48)
  )

  type Global     = GlobalImpl
  type Chromosome = Vec[(Chord, ChordEval)]
  type Generation = GenerationImpl
  type Evaluation = EvaluationImpl
  type Selection  = SelectionImpl
  type Breeding   = BreedingImpl

  def defaultGeneration: Generation = GenerationImpl()
  def defaultEvaluation: Evaluation = SampleEvaluation()
  def defaultSelection : Selection  = SelectionRoulette()
  def defaultBreeding  : Breeding   = BreedingImpl()

  val chromosomeClassTag = reflect.classTag[Chromosome]

  def generationView(init: Generation, config: Config) = AutoView(init, config)
  // def evaluationView(init: Evaluation, config: Config) = AutoView(init, config)
  def selectionView (init: Selection , config: Config) = AutoView(init, config)
  def breedingView  (init: Breeding  , config: Config) = AutoView(init, config)


  def evaluationViewOption = None

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
  override def humanEvaluationSteps = 6

  private def setChromoEditor(c: Chromosome): Unit =
    if (c != null) {
      chordSeqView2.chords = c
    }

  override def chromosomeEditorOption: Option[(Component, () => Chromosome, (Chromosome) => Unit)] =
    Some((chordSeqView2, () => chordSeqView2.chords, setChromoEditor))

  // serialization
  private implicit val fivalformat  = SealedTraitFormat[ForbiddenInterval]
  private implicit val vconsFormat  = SealedTraitFormat[VerticalConstraint]
  private implicit val voiceFormat  = SealedTraitFormat[Voice]
  private implicit val globalFormat = SealedTraitFormat[GlobalImpl]
  // private implicit val alleleFormat = SealedTraitFormat[(Chord, ChordEval)]
  object chromosomeFormat extends Format[Chromosome] {
    def reads(json: JsValue): JsResult[Chromosome] = json match {
      case JsArray(sq) =>
        val res = Try {
          val chromo: Chromosome = sq.map {
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
              (chord, eval)

            case other => sys.error(s"Not a JSON object $other")
          } (breakOut)
          chromo
        }
        res match {
          case Success(c) => JsSuccess(c)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError(s"Not an array $json")
    }

    def writes(c: Chromosome): JsValue = JsArray(
      c.map { case (chord, eval) =>
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
    )
  }
  val generationFormat  = SealedTraitFormat[GenerationImpl]
  val selectionFormat   = SealedTraitFormat[SelectionImpl ]
  private implicit val breedingFunFormat  = SealedTraitFormat[BreedingFunctionImpl]
  val breedingFormat    = SealedTraitFormat[BreedingImpl  ]
  val evaluationFormat  = SealedTraitFormat[EvaluationImpl]
}