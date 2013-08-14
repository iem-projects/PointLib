package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.TickRate

object VerticalDance {
  def apply(startIdx: Int, modelVelo: Boolean, veloCoarse: Int, modelEntry: Boolean,
                             entryCoarse: Double, entryScale: Double, modelInner: Boolean, modelGrains: Boolean,
                             chordSnippets: Vec[Vec[Chord]])(implicit rnd: util.Random): VerticalDance =
    new Impl(startIdx = startIdx, modelVelo = modelVelo, veloCoarse = veloCoarse, modelEntry = modelEntry,
             entryCoarse = entryCoarse, entryScale = entryScale, modelInner = modelInner, modelGrains = modelGrains,
             h = chordSnippets)

  private final class Impl(startIdx: Int, modelVelo: Boolean, veloCoarse: Int, modelEntry: Boolean,
                           entryCoarse: Double, entryScale: Double, modelInner: Boolean, modelGrains: Boolean,
                           h: Vec[Vec[Chord]])(implicit rnd: util.Random) extends VerticalDance {

    private val chordsIn  = h.flatten // flatMap(_._2)
    private val pchSq     = chordsIn.map { c => val pch = c.pitches; pch.head.`class`.step -> pch.last.`class`.step }
    private val recPchF   = ContextDance(pchSq)(pchSq(startIdx) :: Nil)
    private val octSq     = chordsIn.map { c => val pch = c.pitches; (pch.head.midi / 12) -> (pch.last.midi / 12) }
    private val recOctF   = ContextDance(octSq)(octSq(startIdx) :: Nil)
    private val voiceSq   = chordsIn.map(_.size)
    private val recVoiceF = ContextDance(voiceSq)(voiceSq(startIdx) :: Nil)
    private val veloSq    = chordsIn.map { c => val v = (c.avgVelocity + 0.5f).toInt; v - (v % veloCoarse) }
    private val recVeloF  = ContextDance(veloSq)(veloSq(startIdx) :: Nil)

      // a simple occurrence map. frame size -> num voices -> chords
    private lazy val innerOcc = {
      var res = Map.empty[Int, Map[Int, Vec[Chord]]] withDefaultValue (Map.empty withDefaultValue Vector.empty)
      chordsIn.foreach { c =>
        val semi  = c.frameInterval.semitones
        val map0  = res(semi)
        val map   = map0 + (c.size -> (map0(c.size) :+ c))
        res += semi -> map
      }
      res
    }

    // a simple occurrence map. num voices -> chords (velocities)
    private lazy val veloOcc = {
      var res = Map.empty[Int, Vec[Chord]] withDefaultValue Vector.empty
      chordsIn.foreach { c =>
        res = res + (c.size -> (res(c.size) :+ c))
      }
      res
    }

    // a simple occurrence map. num voices -> chords (inner times)
    private lazy val grainOcc = {
      var res = Map.empty[Int, Vec[Chord]] withDefaultValue Vector.empty
      chordsIn.foreach { c =>
        res = res + (c.size -> (res(c.size) :+ c))
      }
      res
    }

    private val entrySq = h.flatMap { cs =>
      cs.sliding(2, 1).to[Vector].flatMap {
        case Seq(a, b) =>
          val fine    = ((b.minOffset - a.maxStop) * 1000 + 0.5).toInt // millis
          val coarse  = (fine * entryCoarse + 0.5).toInt
          if (coarse > 0) Some(fine - fine % coarse) else if (fine > 0) Some(fine) else None
        case _ => Vector.empty
      }
    }

    private val recEntryF = ContextDance(entrySq :+ entrySq.head)(entrySq(startIdx) :: Nil)

    private var produced  = 0

    def move(num: Int): Vector[Chord] = {

    //  val maxVoices = voiceSq.max
    //  val maxFrame  = chordsIn.map(_.frameInterval.semitones).max

      val recPch    = recPchF  .move(num)
      val recOct    = recOctF  .move(num)
      val recVoice  = recVoiceF.move(num)

      val chordsOut0 = ((recPch zip recOct) zip recVoice).zipWithIndex.map {
        case ((((classLo, classHi), (octLo, octHi)), voices), idx) =>

        val off     = (idx + produced) * 0.25
        val pchLo0  = classLo + octLo * 12
        val pchHi0  = classHi + octHi * 12
        val pchLo   = math.min(pchLo0, pchHi0)
        val pchHi   = math.max(pchLo0, pchHi0)
        val noteLo  = OffsetNote(off, pchLo.asPitch, 0.125, 80)

    //    println(s"Note $pchLo $pchHi $voices")

        if (modelInner) {
          var semi  = pchHi - pchLo
          assert(semi >= 0, semi)  // NOT TRUE
          var semiInc = -1
          while (innerOcc(semi).isEmpty) {
            if (semi == 0) semiInc = 1
            semi += semiInc
          }
          val map = innerOcc(semi)
    //      assert(map.nonEmpty)
          var vc    = voices
          assert(vc > 1)
          var vcInc = -1
          while (map(vc).isEmpty) {
            if (vc == 1) vcInc = 1
            vc += vcInc
          }
    //      println("---1")
          val chord   = map(vc).choose
          val ivals   = chord.layeredIntervals.map(_.semitones).scramble
          assert(ivals.sum == semi)
          val chords  = ivals.integrate.map { step =>
            OffsetNote(off, (pchLo + step).asPitch, 0.125, 80)
          }
          Chord(noteLo +: chords)

        } else {
          val noteHi = OffsetNote(off, pchHi.asPitch, 0.125, 80)
          Chord(Vector(noteLo, noteHi))
        }
      }

      val chordsOut1 = if (modelVelo) {
        val recVelo = recVeloF.move(num)
        (chordsOut0 zip recVelo).map { case (c, v) =>
          val vc  = veloOcc(c.size).choose
          val va  = vc.avgVelocity
          val d   = v - va
          Chord((c.notes zip vc.notes).map { case (n, nv) => n.copy(velocity = math.max(1, math.min(127, (nv.velocity + d + 0.5f).toInt))) })
        }

      } else
        chordsOut0

      implicit val rate = TickRate.tempo(120, 1024)

      val chordsOut2 = if (modelGrains) {
        chordsOut1.map { c =>
          val cg = grainOcc(c.size).choose
          val ng = cg.notes.scramble
          val oc = c.minOffset
          val og = cg.minOffset
          val d  = oc - og
          Chord((c.notes zip ng).map { case (na, nb) =>
    //        val d = c.minOffset
            na.copy(offset = nb.offset + d, duration = nb.duration)
          })
        }

      } else {
        chordsOut1
      }

      val chordsOut = if (modelEntry) {
        var off = 0.0
        val recEntry = recEntryF.move(num)
        (chordsOut2 zip recEntry).map { case (c, e) =>
          val delta = off - c.minOffset
          val res = Chord(c.notes.map(n => n.copy(offset = n.offset + delta)))
          off += (e * 0.001 + (c.maxStop - c.minOffset)) * entryScale
          res
        }

      } else
        chordsOut2

      produced += num
      chordsOut
    }
  }
}
sealed trait VerticalDance {
  def move(num: Int): Vector[Chord]
}