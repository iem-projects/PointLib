package at.iem.point.sh.sketches
package genetic

import spire.math.Rational

case class Generation(size: Int = 100, duration: Int = 32, seed: Int /* Long */ = 0) {
  def wholeDur = Rational(duration, 4)
}