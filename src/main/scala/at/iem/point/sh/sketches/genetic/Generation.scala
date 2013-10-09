package at.iem.point.sh.sketches
package genetic

import spire.math.Rational
import de.sciss.muta
import at.iem.point.sh.sketches.genetic.GeneticSystem.{Chromosome, Global}
import scala.util.Random

case class Generation(size: Int = 100, duration: Int = 32, seed: Int /* Long */ = 0)
  extends muta.Generation[Chromosome, Global] {

  def wholeDur = Rational(duration, 4)

  def global: Global = wholeDur

  override def apply(r: Random): Chromosome = ???
}