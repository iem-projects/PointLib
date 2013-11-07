package at.iem.point.ms.sketches

import spire.math.Rational
import scala.annotation.tailrec
import at.iem.point.illism.rhythm.numericCompat
import collection.breakOut

object Tempo {
  private final val DEBUG = false

  private final val r2_3  = Rational(2, 3)
  private final val r3_4  = Rational(3, 4)
  private final val r1_4  = Rational(1, 4)

  //  private final val divisions  = List(1, 2, 4, 8, 16, 32, 64).map(Rational(1, _))
  //  private final val divisions2 = divisions.init
  //  private final val divisions3 = divisions2.init

  type Durations = Vec[Rational]

  /** The result of the algorithm.
    *
    * @param wholeDuration  the duration (unquantized) of a whole note in seconds
    * @param tempoBase      the base unit of tempo, e.g. 1/4
    * @param tempoNom       the nominal tempo in base units per minute, e.g. 120 (with tempoBase = 1/4 meaning that 1/4 = 120)
    * @param durations      the quantized note durations based on the detected tempo
    */
  final case class Result(wholeDuration: Double, tempoBase: Rational, tempoNom: Int, durations: Durations)

  def quantize(durations: Vec[Double], qpm: Double, maxDenom: Int = 32,
            doubleDotted: Boolean = false): Result = {
    require(durations.size >= 1, s"Must have at least one duration (number is ${durations.size})")

    // val _wholeDur   = (60 * 4) / qpm
    val divList     = (0 to 10).map(1 << _).takeWhile(_ <= maxDenom)
    val divisions: List[Rational] = divList.map(Rational(1, _))(breakOut)
    val tup = calcNotes(durations, qpm, divisions, doubleDotted = doubleDotted)
    Result(tup._1, tup._2, tup._3, tup._4.map(_.sum))
  }

  private def calcNotes(durations: Vec[Double], _tempoFrac: Double,
                        _divisions: List[Rational],
                        doubleDotted: Boolean): (Double, Rational, Int, Vec[Durations]) = {
    @tailrec def tempoSig(note: Rational = r1_4): (Rational, Int) = {
      val factor  = 4 * note
      val tempo   = _tempoFrac * factor.doubleValue()
      if (tempo <= 160) { // i.e. maximum 1/4 = 160
        val tempo1  = (tempo + 0.5).toInt + 5 // 9
        val tempo2  = tempo1 - tempo1 % 10  // round to multiples of 10
        (note, tempo2)
      } else {
        val factor = note.numerator.intValue() match {
          case 1 => r3_4  // e.g. 4 -> 8.
          case 3 => r2_3
        }
        tempoSig(note * factor)
      }
    }

    val (_tempoBase, _tempoNom) = tempoSig()

    // val wholeDur  = 60 / (_tempoBase * _tempoNom).doubleValue()
    val _wholeDur   = (60 * 4) / _tempoFrac
    val values      = durations.map(_ / _wholeDur)

    if (DEBUG) {
      println(f"tempoFrac ${_tempoFrac}%1.2f yields base ${_tempoBase} with nominal tempo ${_tempoNom}; wholeDur = ${_wholeDur}%1.2f")
      // println(s"First five onsets frames ${onsets.take(5)} -> seconds ${onsets.take(5)} -> values ${values.take(5)}")
    }

    var gagaismo = true

    val _notes: Vec[Vec[Rational]] = values.map { v =>
      val frac  = Rational(v)
      val lim   = frac.limitDenominatorTo(96)
      // val rest  = lim - note

      @tailrec def decompose(rem: Rational, units: List[Rational], sq: Durations): Durations =
        units match {
          case unit :: tail =>
            if (rem >= unit) {
              decompose(rem - unit, units, sq :+ unit)
            } else {
              decompose(rem, tail, sq)
            }
          case _ => sq
        }

      val dec     = decompose(lim, _divisions, Vector.empty)
      val dotted  = dot(dec, doubleDotted = doubleDotted)

      if (DEBUG && gagaismo) {
        println(s"- first note becomes $lim -- ($frac) -- dotted is $dotted")
        gagaismo = false
      }

      dotted
    }

    (_wholeDur, _tempoBase, _tempoNom, _notes)
  }

  private def dot(sq: Vec[Rational], doubleDotted: Boolean): Vec[Rational] =
    sq match {
      case init :+ a :+ b :+ c if doubleDotted && a.numerator == 1 && a == b * 2 && b == c * 2 => // Doppelpunktierung
        init :+ (a + b + c)
      case init :+ a :+ b      if a.numerator == 1 && a == b * 2               => // Einfachpunktierung
        init :+ (a + b)
      case _ => sq
    }

  /** Tries to guess a "good" tempo from a given sequence of unquantized note offset.
    *
    * @param durations      the unquantized durations in seconds. this must be sorted
    * @param doubleDotted   whether double dotted notes are allowed or not
    * @return               the search result
    */
  def guess(durations: Vec[Double], maxDenom: Int = 32, doubleDotted: Boolean = false): Result = {
    require(durations.size >= 1, s"Must have at least one duration (number is ${durations.size})")

    val minDurSec = durations.min

    val divList     = (0 to 10).map(1 << _).takeWhile(_ <= maxDenom)
    val divisions: List[Rational] = divList.map(Rational(1, _))(breakOut)
    val divisions2  = divisions.init
    val divisions3  = divisions2.init

    // tempo given in beats per minute, where beat = quarter
    @tailrec def autoTempo(tempo: Double = 60): Double = {
      val wholeDur  = 1.0/(tempo/(4 * 60))
      val minValue  = minDurSec / wholeDur
      if (minValue >= 1.0/96) tempo else {
        autoTempo(tempo * 4/3)
      }
    }

    def findBest(): Result = {
      val tempoFrac0  = autoTempo()       // begin with this tempo
      val tempoFrac1  = tempoFrac0 * 3.0  // 1.5  // stop at this tempo
      val tempoFactor = math.pow(2,1.0/128) // increase tempo by this factor in each iteration
      var t = tempoFrac0
      var bestRes: (Double, Rational, Int, Vec[Durations]) = null
      var bestCost1 = Int.MaxValue
      var bestCost2 = Double.PositiveInfinity
      var bestT     = 0.0

      def wooopi(t: Double, _divisions: List[Rational]) {
        val tup   = calcNotes(durations, t, _divisions, doubleDotted = doubleDotted)
        // val cost  = tup._3.map(_.toSet.size).sum  // try to minimise the number of different note values
        val (c1s, c2s) = tup._4.map(d => {
          val sz = d.toSet.size
          val tuplingCost = sz * sz // higher costs for large tuple deconstructions, better to have less tuples
          val dur = d.map(_.toDouble).sum
          val tempoCost = if (dur >= 0.25) dur / 0.25 else 0.25 / dur // higher costs for notes deviating from 1/4
          (tuplingCost, tempoCost)
        }).unzip
        val c1 = c1s.sum // (math.Numeric.IntIsIntegral)
        val c2 = c2s.sum // (math.Numeric.IntIsIntegral)
        if (DEBUG) {
          println(f"With tempo $t, cost is $bestCost1 / $bestCost2%1.2f")
        }
        if (c1 < bestCost1 || (c1 == bestCost1 && c2 < bestCost2)) {
          bestRes   = tup
          bestCost1 = c1
          bestCost2 = c2
          bestT     = t
          if (DEBUG) {
            println("...new best")
          }
        }
      }

      while (t <= tempoFrac1) {
        wooopi(t, divisions)
        t *= tempoFactor
      }

      // println("double trouble...")
      wooopi(bestT * 2, divisions2)
      wooopi(bestT * 3, divisions2)
      wooopi(bestT * 4, divisions3)

      Result(bestRes._1, bestRes._2, bestRes._3, bestRes._4.map(_.sum))
    }

    findBest()
  }
}