package at.iem.point.sh.sketches
package genetic

import de.sciss.muta
import at.iem.point.sh.sketches.genetic.GeneticSystem.{Chromosome, Global}
import scala.util.Random

case class GenerationImpl(global: Global = GlobalImpl(), size: Int = 100, seed: Int = 0)
  extends muta.Generation[Chromosome, Global] {

  // def duration = global.duration

  override def apply(r: Random): Chromosome = Fitness.randomSequence(global)(r)
}