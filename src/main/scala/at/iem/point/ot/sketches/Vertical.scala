package at.iem.point.ot.sketches

import at.iem.point.illism._
import scala.util.Random

object Vertical extends App {
  // - erlaubte intervalle
  // - erlaubte intervall konstellationen
  // - was ist mit register? gehoert zur stimmfuehrung? (ja)

  // algorithmus:
  // - gegeben grundton
  // - gegeben N zahl der stimmen
  // - generiere N-1 intervalle

  // das erste intervall koennte aus dem pool aller vorkommenden intervalle genommen werden
  // (a) zufaellig; (b) zufaellig aber gewichtet nach frequenz
  // das nachfolgende intervall wuerde aus pool der xkorr genommen werden
  // (a) zufaellig; (b) zufaellig aber gewichtet nach frequenz

  // dann wuerden akkorde weggeworfen werden muessen, wenn all-intervalle dadurch entstehen, die nicht im korpus sind

  def generate(base: Pitch, voices: Int)(implicit r: Random): Chord = {
    ???
  }
}