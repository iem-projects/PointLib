package at.iem.point.ot.sketches

import de.sciss.muta
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import at.iem.point.illism._
import scala.util.Random
import de.sciss.muta.{SelectionNumber, SelectionPercent, SelectionSize}
import scala.swing.Label
import scala.annotation.tailrec

object GeneticSystem extends muta.System {
  type Chromosome = Vec[Chord]

  /** @param voices   the number of voices (vertical chord size)
    * @param length   the number of chords to produce
    */
  case class Global(voices: Int = 5, length: Int = 12)

  case class Generation(size: Int = 400, global: Global = Global(), seed: Int = 0)
    extends muta.Generation[Chromosome, Global] {

    def apply(r: Random): Chromosome =
      Vec.fill(global.length)(Vertical.generate(voices = global.voices, base = 48.asPitch)(r))
  }

  sealed trait Evaluation extends muta.Evaluation[Chromosome]

  case class SampleEvaluation(maxUp: Int = 2, maxDown: Int = 2, lowest: Int = 36, highest: Int = 96) extends Evaluation {
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

    def apply(c: Chromosome): Double = {
      val lines = Horizontal.fromChords(c)
      lines.map(evalLine).mean
    }
  }

  sealed trait Selection extends muta.Selection[Chromosome]

  case class SelectionRoulette(size: SelectionSize = SelectionPercent(33))
    extends muta.impl.SelectionRouletteImpl[Chromosome] with Selection

  case class SelectionTruncation(size: SelectionSize = SelectionPercent(20))
    extends muta.impl.SelectionTruncationImpl[Chromosome] with Selection

  sealed trait BreedingFunction extends muta.BreedingFunction[Chromosome, Global]

  case class Breeding(elitism        : SelectionSize    = SelectionNumber(20),
                     crossoverWeight: SelectionPercent  = SelectionPercent(50),
                     crossover      : BreedingFunction  = Crossover,
                     mutation       : BreedingFunction  = Mutation())
    extends muta.impl.BreedingImpl[Chromosome, Global]

  object Crossover extends muta.impl.CrossoverVecImpl[Chord, Global] with BreedingFunction

  case class Mutation(chords: SelectionSize = SelectionPercent(20),
                      voices: SelectionSize = SelectionNumber(3), interval: Int = 7)
    extends muta.impl.MutationVecImpl [Chord, Global] with BreedingFunction {

    override protected val numGenesSize = chords

    def mutate(gene: Chord)(implicit r: util.Random): Chord = {
      // println("Muta!")

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
      gene.copy(notes = notes.sortBy(_.pitch))
    }
  }

  def defaultGeneration: Generation = Generation()
  def defaultEvaluation: Evaluation = SampleEvaluation()
  def defaultSelection : Selection  = SelectionRoulette()
  def defaultBreeding  : Breeding   = Breeding()

  val chromosomeClassTag = reflect.classTag[Chromosome]

  def generationView(init: Generation, config: Config) = AutoView(init, config)
  def evaluationView(init: Evaluation, config: Config) = AutoView(init, config)
  def selectionView (init: Selection , config: Config) = AutoView(init, config)
  def breedingView  (init: Breeding  , config: Config) = AutoView(init, config)

  private val chordSeqView = new ChordSeqView

  override def chromosomeView(c: Chromosome, default: Label, selected: Boolean, focused: Boolean) = {
    // default.text = c.map(_.pitches.mkString("[", " ", "]")).mkString(" ")
    // default
    chordSeqView.chords = c
    chordSeqView
  }
}