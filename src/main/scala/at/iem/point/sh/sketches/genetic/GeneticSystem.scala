package at.iem.point.sh.sketches
package genetic

import de.sciss.muta
import at.iem.point.illism.rhythm.Cell
import de.sciss.play.json.AutoFormat
import play.api.libs.json.Format
import de.sciss.guiflitz.AutoView.Config
import de.sciss.guiflitz.AutoView
import scala.swing.{Graphics2D, Label}
import at.iem.point.sh.sketches.gui.ChromosomeView
import javax.swing.Icon
import java.awt.Graphics

object GeneticSystem extends muta.System {
  type Chromosome = Vec[Cell]
  type Generation = GenerationImpl
  type Evaluation = EvaluationImpl
  type Selection  = SelectionImpl
  type Breeding   = BreedingImpl

  type Global     = GlobalImpl

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

  override def chromosomeView(c: Chromosome, default: Label, selected: Boolean, focused: Boolean): swing.Component = {
    // if (c != null) ViewComponent.chromosome = c
    // ViewComponent
    val cn = c.map(_.normalized)
    val sz = ChromosomeView.preferredSize(cn)
    default.text == null
    default.icon = new Icon {
      def getIconWidth  = sz.width
      def getIconHeight = sz.height

      def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
        g.translate(x, y)
        val widthDur: Double = 9 // XXX TODO: need global - duration.toDouble * 1.1
        ChromosomeView.paint(cn, g.asInstanceOf[Graphics2D],
          default.peer.getWidth  - x,
          default.peer.getHeight - y, widthDur)
        g.translate(-x, -y)
      }
    }
    default
  }
}