package at.iem.point.sh.sketches
package genetic

import collection.immutable.{IndexedSeq => Vec}
import Fitness._

/** An urn (or bag) is a collection from which random elements are drawn. It is automatically refilled when empty. */
final class Urn[A](init: Vec[A]) {
  private var rem = init

  def apply()(implicit random: util.Random): A = {
    if (rem.isEmpty) rem = init
    val i   = random.nextInt(rem.size)
    val res = rem(i)
    rem     = rem.removeAt(i)
    res
  }
}