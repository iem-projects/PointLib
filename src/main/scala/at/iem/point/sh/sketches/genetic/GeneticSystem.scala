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
  type Generation = genetic.Generation
  type Evaluation = genetic.Evaluation
  type Selection  = genetic.Selection
  type Breeding   = genetic.Breeding

  type Global     = Rational

  //  implicit val rationalFormat     : Format[Rational]        = AutoFormat[Rational]
  //  implicit val noteOrRestFormat   : Format[NoteOrRest]      = AutoFormat[NoteOrRest]
  //  implicit val notesOrRestsFormat : Format[Vec[NoteOrRest]] = AutoFormat[Vec[NoteOrRest]]
  //  implicit val cellFormat         : Format[Cell]            = AutoFormat[Cell]

  def defaultGeneration: Generation = genetic.Generation()
  def defaultEvaluation: Evaluation = ??? // FrameIntervalEval()
  def defaultSelection : Selection  = ??? // SelectionRoulette()
  def defaultBreeding  : Breeding   = genetic.Breeding()

  val chromosomeFormat : Format[Vec[Cell]]  = AutoFormat[Vec[Cell]]
  val breedingFormat   : Format[Breeding]   = ???
  val evaluationFormat : Format[Evaluation] = ???
  val generationFormat : Format[Generation] = ???
  val selectionFormat  : Format[Selection]  = ???

  val chromosomeClassTag = reflect.classTag[Chromosome]

  def generationView(init: Generation, config: Config) = AutoView(init, config)
  def evaluationView(init: Evaluation, config: Config) = AutoView(init, config)
  def selectionView (init: Selection , config: Config) = AutoView(init, config)
  def breedingView  (init: Breeding  , config: Config) = AutoView(init, config)
  def evaluationViewOption = Some(evaluationView)
}