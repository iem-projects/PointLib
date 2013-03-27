package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.midi.{Sequencer, Sequence, Track, TickRate}

object Evolutions3 extends App {
  val NUM         = 200
  val SEED        = 2L      // seed of the random number generator
  val START       = 2       // start index in the pitch sequence to begin wih
  val VELO        = true    // model velocity
  val VELO_COARSE = 4       // velocity rasterisation (in steps)
  val ENTRY       = true    // model entry offsets
  val ENTRY_COARSE= 0.2     // entry offset rasterisation (relative, in percent 0...1)
  val ENTRY_SCALE = 1.5     // slow down factor if using entry modelling

  val INNER       = true    // model inner vertical structure of chords (`true`) or just frame intervals (`false`)
  val GRAINS      = true    // model inner horizontal structure of chords (`true`) or just produce monolithic blocks (`false`)

  val snippet   = improvSnippets.last  // staticChords(5).head
  val sq        = loadSnippet(snippet) // .head
  val notesIn0  = sq.notes
  val (_, h)    = NoteUtil.splitMelodicHarmonic(notesIn0)
  val chordsIn  = h.flatMap(_._2)

  val pchSq     = chordsIn.map { c => val pch = c.pitches; pch.head.`class`.step -> pch.last.`class`.step }
  val recPch    = ContextDance.move(pchSq, num = NUM, seed = SEED)(pchSq(START) :: Nil)
  val octSq     = chordsIn.map { c => val pch = c.pitches; (pch.head.midi / 12) -> (pch.last.midi / 12) }
  val recOct    = ContextDance.move(octSq, num = NUM, seed = SEED)(octSq(START) :: Nil)
  val voiceSq   = chordsIn.map(_.size)
  val recVoice  = ContextDance.move(voiceSq, num = NUM, seed = SEED)(voiceSq(START) :: Nil)
//  val maxVoices = voiceSq.max
//  val maxFrame  = chordsIn.map(_.frameInterval.semitones).max
  implicit val rnd = new util.Random(SEED)

  // a simple occurrence map. frame size -> num voices -> chords
  var inner     = Map.empty[Int, Map[Int, IIdxSeq[Chord]]] withDefaultValue (Map.empty withDefaultValue Vector.empty)
  chordsIn.foreach { c =>
    val semi  = c.frameInterval.semitones
    val map0  = inner(semi)
    val map   = map0 + (c.size -> (map0(c.size) :+ c))
    inner += semi -> map
  }

  val chordsOut0 = ((recPch zip recOct) zip recVoice).zipWithIndex.map {
    case ((((classLo, classHi), (octLo, octHi)), voices), idx) =>

    val off     = idx * 0.25
    val pchLo0  = classLo + octLo * 12
    val pchHi0  = classHi + octHi * 12
    val pchLo   = math.min(pchLo0, pchHi0)
    val pchHi   = math.max(pchLo0, pchHi0)
    val noteLo  = OffsetNote(off, pchLo.asPitch, 0.125, 80)

//    println(s"Note $pchLo $pchHi $voices")

    if (INNER) {
      var semi  = pchHi - pchLo
      assert(semi >= 0, semi)  // NOT TRUE
      var semiInc = -1
      while (inner(semi).isEmpty) {
        if (semi == 0) semiInc = 1
        semi += semiInc
      }
      val map = inner(semi)
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

  val chordsOut1 = if (VELO) {
    // a simple occurrence map. num voices -> chords (velocities)
    var velos   = Map.empty[Int, IIdxSeq[Chord]] withDefaultValue Vector.empty
    chordsIn.foreach { c =>
      velos = velos + (c.size -> (velos(c.size) :+ c))
    }
    val veloSq  = chordsIn.map { c => val v = (c.avgVelocity + 0.5f).toInt; v - (v % VELO_COARSE) }
    val recVelo = ContextDance.move(veloSq, num = NUM, seed = SEED)(veloSq(START) :: Nil)
    (chordsOut0 zip recVelo).map { case (c, v) =>
      val vc  = velos(c.size).choose
      val va  = vc.avgVelocity
      val d   = v - va
      Chord((c.notes zip vc.notes).map { case (n, nv) => n.copy(velocity = math.max(1, math.min(127, (nv.velocity + d + 0.5f).toInt))) })
    }

  } else
    chordsOut0

  implicit val rate = TickRate.tempo(120, 1024)

  val chordsOut2 = if (GRAINS) {
    // a simple occurrence map. num voices -> chords (inner times)
    var grains = Map.empty[Int, IIdxSeq[Chord]] withDefaultValue Vector.empty
    chordsIn.foreach { c =>
      grains = grains + (c.size -> (grains(c.size) :+ c))
    }
    chordsOut1.map { c =>
      val cg = grains(c.size).choose
      val ng = cg.notes.scramble
      val oc = c.minOffset
      val og = cg.minOffset
      val d  = oc - og
      Chord((c.notes zip ng).map { case (na, nb) =>
        val d = c.minOffset
        na.copy(offset = nb.offset + d, duration = nb.duration)
      })
    }

  } else {
    chordsOut1
  }

  val chordsOut = if (ENTRY) {
    val entrySq = h.flatMap { case (_, cs) =>
      cs.sliding(2, 1).to[Vector].flatMap {
        case Seq(a, b) =>
          val fine    = ((b.minOffset - a.maxStop) * 1000 + 0.5).toInt // millis
          val coarse  = (fine * ENTRY_COARSE + 0.5).toInt
          if (coarse > 0) Some(fine - fine % coarse) else if (fine > 0) Some(fine) else None
        case _ => Vector.empty
      }
    }

//    println(entrySq.mkString(", "))
//    ContextDance.DEBUG = true
    val recEntry = ContextDance.move(entrySq :+ entrySq.head, num = NUM, seed = SEED)(entrySq(START) :: Nil)
//    println(recEntry.mkString(", "))
    var off = 0.0
    (chordsOut2 zip recEntry).map { case (c, e) =>
      val delta = off - c.minOffset
      val res = Chord(c.notes.map(n => n.copy(offset = n.offset + delta)))
      off += (e * 0.001 + (c.maxStop - c.minOffset)) * ENTRY_SCALE
      res
    }

  } else
    chordsOut2

  val events  = chordsOut.flatMap(_.toMIDI)
  val track   = Track(events)
  val sqOut   = Sequence(Vector(track))

  sqOut.writeFile(outPath /
    (s"Snippet${snippet}_ChordSeq_${START}_${SEED}${if (INNER) "I" else ""}" +
     s"${if (GRAINS) "G" else ""}${if (VELO) "V" else ""}${if (ENTRY) "E" else ""}.mid"))

  val player  = Sequencer.open()
  player.play(sqOut)
  Thread.sleep(((track.duration + 1) * 1000).toLong)
  player.stop()
  player.close()
}