package at.iem.point.er.sketches

import scala.swing.{Component, Swing}
import de.sciss.audiowidgets.{TimelineModel, Transport}
import java.io.File
import de.sciss.synth
import synth._
import io.SampleFormat
import Swing._
import Ops._
import de.sciss.audiowidgets.Transport._
import de.sciss.osc.{Bundle, Message}
import de.sciss.model.Change
import de.sciss.desktop.Implicits._
import javax.swing.KeyStroke
import de.sciss.desktop.FocusType

class PlayerView(doc: Document, timelineModel: TimelineModel) {
  import doc.{file     => inputFile}
  import doc.{fileSpec => inputSpec}

  private val sys = AudioSystem.instance

  private var _position = timelineModel.position

  private var playing   = Option.empty[Playing]
  private var _diskAmp  = 1.0f

  def diskAmp: Float = _diskAmp
  def diskAmp_=(value: Float): Unit = {
    _diskAmp = value
    playing.foreach(_.synth.set("diskAmp" -> value))
  }

  private var _resynthAmp = 1.0f

  def resynthAmp: Float = _resynthAmp
  def resynthAmp_=(value: Float): Unit = {
    _resynthAmp = value
    playing.foreach(_.synth.set("resynthAmp" -> value))
  }

  private def position = _position
  private def position_=(frame: Long): Unit = {
    _position = frame
    timelineModel.modifiableOption.foreach(_.position = frame)
  }

  private var pitches: PitchAnalysis.Product = doc.pitches
  //  def pitches = _pitches
  //  def pitches_=(seq: PitchAnalysis.Product) {
  //    _pitches = seq
  //    val p = isPlaying
  //    if (p) {
  //      stop()
  //      play()
  //    }
  //  }

  private var onsets = doc.onsets
  //  def onsets = _onsets
  //  def onsets_=(seq: MultiResOnsets) {
  //    _onsets = seq
  //    val p = isPlaying
  //    if (p) {
  //      stop()
  //      play()
  //    }
  //  }

  doc.addListener {
    case Document.OnsetsChanged(_, Change(_, now)) =>
      onsets = now
      val p = isPlaying
      if (p) {
        stop()
        play()
      }

    case Document.PitchesChanged(_, Change(_, now)) =>
      pitches = now
      val p = isPlaying
      if (p) {
        stop()
        play()
      }
  }
  timelineModel.addListener {
    case TimelineModel.Position(_, Change(_, now)) if now != _position =>
      val p = isPlaying
      if (p) stop()
      _position = now
      if (p) play()
  }

  def isPlaying = playing.isDefined

  private final case class Playing(synth: Synth, pitchBuf: Buffer, onsetsBuf: Buffer)

  private def mkOnsetsEnv(pos: Long): Vec[Float] = {
    // XXX TODO: forgot about pos right now....
    val sr = inputSpec.sampleRate
    // XXX TODO: should have different volume for different onset strengths
    val frameDurs = (0L +: onsets.onsets.map(_.pos) :+ inputSpec.numFrames).sliding(2,1).map { case Seq(start: Long, stop: Long) => stop - start }
    frameDurs.map(fr => (fr / sr).toFloat).toIndexedSeq
  }

  private def mkPitchEnv(pos: Long): Vec[Float] = {
    val p0         = pitches
    val numFr     = inputSpec.numFrames
    // make sure we begin at frame zero
    val p1        = if (p0.nonEmpty && p0.head.start == 0L) p0 else {
      PitchAnalysis.Sample(0L, p0.headOption.map(_.start).getOrElse(numFr), CurveFitting.PointFit(0f), 0f) +: p0
    }
    // make sure we end at numFrames
    val p         = if (p1.last.stop >= numFr) p1 else {
      p1 :+ PitchAnalysis.Sample(p1.last.stop, numFr, CurveFitting.PointFit(0f), 0f)
    }
//p.foreach(println)

    // (startFreq, startClar) followed by flat tuple4 sequence (dur, shape, segFreq, segClar)
    val seqB      = Vector.newBuilder[Float]
    var lastStop  = 0L
    var seqSz     = 0
    val sr        = inputSpec.sampleRate
    seqB += 0f  // initial frequency
    seqB += 0f  // initial clarity

    def add(smp: PitchAnalysis.Sample): Unit = {
      val start     = if (smp.start <= lastStop) lastStop + 1 else smp.start
      val stop      = math.max(start + 1, smp.stop)
      val gapFrames = start - 1 - lastStop
      if (gapFrames > 0) { // insert 'gap'
        seqB += (gapFrames / sr).toFloat
        seqB += Curve.step.id
        seqB += 0f
        seqB += 0f
        seqSz += 1
        lastStop += gapFrames
      }
      smp.freq match {
        case CurveFitting.PointFit(freq) =>
          val pitchFrames = stop - lastStop
          seqB += (pitchFrames / sr).toFloat
          seqB += Curve.step.id
          seqB += freq.toFloat
          seqB += smp.clarity
          seqSz += 1
          lastStop += pitchFrames

        case lin @ CurveFitting.LinearFit(_, startFreq) =>
          val pitchFrames = stop - start
          val stopFreq    = lin(smp.stop - smp.start)
          seqB += (1 / sr).toFloat
          seqB += Curve.step.id
          seqB += startFreq.toFloat
          seqB += smp.clarity
          seqSz += 1
          lastStop += 1

          seqB += (pitchFrames / sr).toFloat
          seqB += Curve.lin.id
          seqB += stopFreq.toFloat
          seqB += smp.clarity
          seqSz += 1
          lastStop += pitchFrames

        case CurveFitting.QuadraticFit(_, _, _, _) => ???
      }
    }

    var posIdx  = -1
    p.foreach { smp =>
      if (smp.start == pos) posIdx = seqSz
//      if (pos > lastStop && pos < smp.start) {  // insert dummy break point at current start frame
////        add(pos - lastStop, 0f, 0f)
////        add(smp.start - pos, 0f, 0f)
//        posIdx = seqSz
//      } else {
        add(smp)
//      }
    }
    val seq0 = seqB.result()
posIdx = 0  // XXX TODO
    require(posIdx >= 0)
    if (posIdx == 0) seq0 else seq0.drop(posIdx * 4) ++ seq0.take(posIdx * 4)  // "rotate"
  }

  def goToBegin(): Unit = {
    stop()
    position = 0L
    // axis.repaint()
  }

  def stop(): Unit = {
    if (sys.isRunning) {
      playing.foreach { p =>
        p.synth.free()
//        p.pitchBuf.free()
//        p.onsetsBuf.free()
      }
    }
    playing = None
    updateStopPlay()
  }

  def play(): Unit = {
    stop()
    sys.server match {
      case Some(s: Server) =>
        val (_, msg) = play(s, loop = true)
        s ! msg
      case _ =>
    }
    updateStopPlay()
  }

  def capture(f: File): Unit = {
    stop()
    goToBegin()
    sys.server match {
      case Some(s: Server) =>
        val (syn, playMsg) = play(s, loop = false)

        val diskBuf   = Buffer(s)
        val diskAlloc = diskBuf.allocMsg(numFrames = 32768, numChannels = 2)
        val diskWrite = diskBuf.writeMsg(f.getAbsolutePath,
          sampleFormat = SampleFormat.Int16, numFrames = 0, leaveOpen = true)

        val recDf = SynthDef("$point_rec") {
          import ugen._
          val sig = Limiter.ar(In.ar(0, 2))
          DiskOut.ar("buf".kr, sig)
        }

        val diskSyn = Synth(s)

        val diskNew = diskSyn.newMsg(recDf.name, target = s, addAction = addToTail, args = Seq("buf" -> diskBuf.id))
        val recDfRcv = recDf.recvMsg(diskNew)

        syn.onEnd {
          diskSyn.free()
          diskBuf.close()
          diskBuf.free()
        }

//        s.dumpOSC(Dump.Text)
        s ! Bundle.now(diskAlloc, diskWrite, recDfRcv, playMsg)

      case _ =>
    }
    updateStopPlay()
  }

  private def play(s: Server, loop: Boolean): (Synth, Message) = {
    val diskBuf = Buffer(s)
    import inputSpec.{numChannels, numFrames}
    val df      = synthDef(numChannels)
    val syn     = Synth(s)
    val resp    = message.Responder(s) {
      case Message("/tr", syn.id, 0, pos: Float) => Swing.onEDT {
        position = (pos + 0.5).toLong
        // axis.repaint()
      }
    }
    val start   = math.min(_position, numFrames - 32768).toInt
    val pchEnv  = mkPitchEnv(start)
    val onsEnv  = mkOnsetsEnv(start)
//println(pchEnv)
    val pitchBuf  = Buffer(s)
    val onsetsBuf = Buffer(s)
//println("PATH = " + inputFile.getAbsolutePath)
    val newMsg    = syn.newMsg(df.name, args = Seq(
      "diskBuf" -> diskBuf.id, "numFrames" -> numFrames.toFloat, "startFrame" -> start.toFloat,
      "diskAmp" -> diskAmp, "resynthAmp" -> resynthAmp,
      "pitchBuf" -> pitchBuf.id, "onsetsBuf" -> onsetsBuf.id, "loop" -> (if (loop) 1f else 0f)
    ))
//    // stupid dummy generation because of buffer state being checked in setnMsg...
//    pitchBuf.allocMsg(numFrames = pchEnv.size, numChannels = 1)

    val onsetsAllocMsg = onsetsBuf.allocMsg(numFrames = onsEnv.size, numChannels = 1, completion = Bundle.now(
      onsetsBuf.setnMsg(onsEnv),
      newMsg
    ))

    val pitchAllocMsg = pitchBuf.allocMsg(numFrames = pchEnv.size, numChannels = 1, completion = onsetsAllocMsg)

//        val syncMsgs  = Bundle.now(setEMsg, newMsg)
    val diskCueMsg    = diskBuf.cueMsg(path = inputFile.getAbsolutePath, startFrame = start, completion = pitchAllocMsg)
    val diskAllocMsg  = diskBuf.allocMsg(numFrames = 32768, numChannels = numChannels, completion = diskCueMsg)
    val recvMsg       = df.recvMsg(completion = diskAllocMsg)
    syn.onEnd {
      diskBuf.close(); diskBuf.free()
      pitchBuf.free(); onsetsBuf.free()
      resp.remove()
    }
    playing = Some(Playing(syn, pitchBuf = pitchBuf, onsetsBuf = onsetsBuf))

//    s.dumpOSC(Dump.Text)

//    s ! recvMsg
    resp.add()
    (syn, recvMsg)
  }

  private def synthDef(numChannels: Int) = SynthDef("disk_" + numChannels) {
    import ugen._
    val numFr   = "numFrames".ir
    val phasor  = (Phasor.ar(hi = numFr) + "startFrame".ir) % numFr // sucky resetVal doesn't work
//    val phasImp = Impulse.ar(SampleRate.ir/numFr)
    val pTrig   = Impulse.kr(20)
    val disk0   = DiskIn.ar(numChannels, "diskBuf".ir, loop = 1)
    val disk    = Mix.mono(disk0) * "diskAmp".kr(1)
    val pSmp    = A2K.kr(phasor)
    SendTrig.kr(pTrig, id = 0, value = pSmp)

    val pitchBuf = "pitchBuf".ir
//    val envSz4 = envSz * 4

    val loop        = "loop".ir
    val doneAction  = loop.linlin(0, 1, freeSelf.id, doNothing.id)

    val pitchFreq = Dbufrd(pitchBuf, Dseries(0, 4, inf), loop = loop)
    val pitchClar = Dbufrd(pitchBuf, Dseries(1, 4, inf), loop = loop)
    def pitchDur()= Dbufrd(pitchBuf, Dseries(2, 4, inf), loop = loop)  // def!
    val pitchShp  = Dbufrd(pitchBuf, Dseries(3, 4, inf), loop = loop)
    val freq      = DemandEnvGen.ar(levels = pitchFreq, durs = pitchDur(), shapes = pitchShp, doneAction = doneAction)
    val clar      = DemandEnvGen.ar(levels = pitchClar, durs = pitchDur(), shapes = Curve.step.id)
//    freq.poll(2, label = "f")
//    clar.poll(2, label = "c")

    val onsetsBuf = "onsetsBuf".ir
    val onsetsDur = Dbufrd(onsetsBuf, Dseries(0, 1, inf), loop = loop)
    val onsetsEnv = DemandEnvGen.ar(levels = Dseq(Seq(1, 0), inf), durs = onsetsDur, shapes = Curve.step.id)
// onsetsEnv.poll(10)
    val onsets    = onsetsEnv absdif Delay1.ar(onsetsEnv)

    val freqL         = Gate.ar(freq, clar)
    val piano         = pianoFunc(freqL) * LagUD.ar(clar, 0.02, 0.1)
    val pitchResynth  = Mix.mono(verb(piano))
    val onsetsResynth = Mix.mono(clickFunc(onsets))
    val resyn = (pitchResynth + onsetsResynth) * "resynthAmp".kr(1)

    val sig: GE = Seq(disk, resyn)

    Out.ar(0, sig)
  }

  private def updateStopPlay(): Unit = {
    val b = isPlaying
    transportStrip.button(Transport.Stop).foreach(_.selected = !b)
    transportStrip.button(Transport.Play).foreach(_.selected =  b)
  }

//  private def updateTime() {
//
//  }
//
//  private val ggTime = new Label {
//
//  }

  private lazy val transportStrip = {
    val res = makeButtonStrip(Seq(
      GoToBegin(goToBegin()),
      Stop(stop()),
      Play(play())
    ))
    val (Some(ggStop), Some(ggStart)) = (res.button(Stop), res.button(Play))
    res.addAction("play-stop", new swing.Action(null) {
      accelerator = Some(KeyStroke.getKeyStroke(' '))

      def apply(): Unit = {
        val b = if (isPlaying) ggStop else ggStart
        b.doClick()
      }
    }, FocusType.Window)
    res
  }

  lazy val transport: Component = /* new BoxPanel(Orientation.Horizontal) {
    contents += */
    transportStrip
/*    contents += HStrut(16)
  }
*/
  stop()

  private def pianoFunc(freq: GE): GE = {
    import ugen._
    val exc   = BrownNoise.ar(0.007 /* Seq(0.007,0.007) */) * LFNoise1.kr(ExpRand(0.125,0.5)).madd(0.6, 0.4).max(0)
//    val specs = KlangSpec.tabulate(12) { i =>
//      (freq * (i + 1), Rand(0.7, 0.9), Rand(1.0, 3.0))
//    }
//    (Klank.ar(specs = specs, in = exc) * 0.1).softclip
    Mix.tabulate(12) { i =>
      Ringz.ar(in = exc, freq = freq.max(30) * (i + 1), attack = Rand(1.0, 3.0), decay = Rand(0.7, 0.9))
    }
  }

  private def clickFunc(trig: GE): GE = {
    import ugen._
    val p = 15 // number of partials per channel per 'cymbal'.
    val f1 = 1433 * 0.5f // 555; // 500 + 2000.0.rand;
    val f2 = 311 * 0.5f // 3555; // 8000.0.rand;
    Vector.fill(2) {
      val spec = KlangSpec.fill(p) {
        (Constant(f1) + math.random * f2, 1.0, 1.0)
      }
      Klank.ar(spec, Decay.ar(trig, 0.004) * WhiteNoise.ar(0.03))
    }
  }


  private def verb(in: GE): GE = {
    import ugen._
    val roomSize    = 100
    val revTime     = 4
    val damping     = 0.62
    val inputBW     = 0.48
    val spread      = 15
    val dryLevel    = -12
    val earlyLevel  = -11
    val tailLevel   = -6

    val a = Mix.mono(in)
    val v = GVerb.ar(in = a,
        roomSize      = roomSize,
        revTime       = revTime,
        damping       = damping,
        inputBW       = inputBW,
        spread        = spread,
        dryLevel      = dryLevel.dbamp,
        earlyRefLevel = earlyLevel.dbamp,
        tailLevel     = tailLevel.dbamp,
        maxRoomSize   = roomSize)
    v * 0.3 + a
  }
}