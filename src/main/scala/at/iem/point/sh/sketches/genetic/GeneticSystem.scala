package at.iem.point.sh.sketches
package genetic

import de.sciss.muta
import at.iem.point.illism.rhythm.Cell
import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import spire.math.Rational

object GeneticSystem extends muta.System {
  type Chromosome = Vec[Cell]
  type Generation = GenerationImpl
  type Evaluation = EvaluationImpl
  type Selection  = SelectionImpl
  type Breeding   = BreedingImpl

  type Global     = Rational

  //  implicit val rationalFormat     : Format[Rational]        = AutoFormat[Rational]
  //  implicit val noteOrRestFormat   : Format[NoteOrRest]      = AutoFormat[NoteOrRest]
  //  implicit val notesOrRestsFormat : Format[Vec[NoteOrRest]] = AutoFormat[Vec[NoteOrRest]]
  //  implicit val cellFormat         : Format[Cell]            = AutoFormat[Cell]

  def defaultGeneration: Generation = GenerationImpl()
  def defaultEvaluation: Evaluation = EvalWindowed() // EvalSerial()
  def defaultSelection : Selection  = SelectionRoulette()
  def defaultBreeding  : Breeding   = BreedingImpl()

  val chromosomeFormat : Format[Vec[Cell]]  = AutoFormat[Vec[Cell]]
  val selectionFormat  : Format[Selection]  = SelectionImpl.format // AutoFormat[SelectionImpl]
  val breedingFormat   : Format[Breeding]   = AutoFormat[BreedingImpl]
  val generationFormat : Format[Generation] = AutoFormat[GenerationImpl]
  implicit lazy val evaluationFormat: Format[Evaluation] = AutoFormat[EvaluationImpl] // recursive!

  val chromosomeClassTag = reflect.classTag[Chromosome]

  def generationView(init: Generation, config: Config) = AutoView(init, config)
  def evaluationView(init: Evaluation, config: Config) = AutoView(init, config)
  def selectionView (init: Selection , config: Config) = AutoView(init, config)
  def breedingView  (init: Breeding  , config: Config) = AutoView(init, config)
  def evaluationViewOption = Some(evaluationView)
}