package at.iem.point.er.sketches

import scala.swing.{Component, Label, Swing, Panel, Orientation, BoxPanel, BorderPanel}
import de.sciss.audiowidgets.{LCDPanel, Transport, Axis}
import java.io.File
import java.awt.{RenderingHints, Color, Graphics2D}
import de.sciss.synth
import synth._
import io.AudioFileSpec
import java.awt.geom.GeneralPath
import Swing._
import Ops._
import de.sciss.audiowidgets.Transport._
import scala.swing.event.MouseClicked
import de.sciss.osc.Message

class PlayerView(inputFile: File, inputSpec: AudioFileSpec) {
  private val sys = AudioSystem.instance

  private var position = 0L

  private var playing   = Option.empty[Playing]
  private var _diskAmp  = 1.0f

  def diskAmp: Float = _diskAmp
  def diskAmp_=(value: Float) {
    _diskAmp = value
    playing.foreach(_.synth.set("diskAmp" -> value))
  }

  private var _resynthAmp = 1.0f

  def resynthAmp: Float = _resynthAmp
  def resynthAmp_=(value: Float) {
    _resynthAmp = value
    playing.foreach(_.synth.set("resynthAmp" -> value))
  }

  private final case class Playing(synth: Synth /* , resp: osc.Responder */)

  lazy val axis: Axis = new Axis {
    format  = Axis.Format.Time(hours = false, millis = true)
    maximum = inputSpec.numFrames / inputSpec.sampleRate

    val tri = new GeneralPath()
    tri.moveTo(-6, 0)
    tri.lineTo(7, 0)
    tri.lineTo(0, 13)
    tri.closePath()

    val colr = new Color(0x00, 0x00, 0xFF, 0x80)

    listenTo(mouse.clicks)
    reactions += {
      case MouseClicked(c, pt, _, _, _) =>
        val p = playing.isDefined
        if (p) stop()
        position = math.max(0L, math.min(inputSpec.numFrames,
          pt.x.toDouble.linlin(0, c.size.width, 0, inputSpec.numFrames))).toLong
        if (p) play()
        c.repaint()
    }

    override protected def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      val x = position.toDouble.linlin(0, inputSpec.numFrames, 0, size.width)
      g.setColor(colr)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val atOrig = g.getTransform
      g.translate(x, 0)
      g.fill(tri)
      g.setTransform(atOrig)
    }
  }

  def goToBegin() {
    stop()
    position = 0L
    axis.repaint()
  }

  def stop() {
    if (sys.isRunning) {
      playing.foreach { p =>
        p.synth.free()
//        p.resp.remove()
      }
    }
    playing = None
    updateStopPlay()
  }

  def play() {
    stop()

    sys.server match {
      case Some(s: Server) =>
        val buf   = Buffer(s)
        import inputSpec.{numChannels, numFrames}
        val df    = synthDef(numChannels)
        val syn   = Synth(s)
        val resp  = osc.Responder(s) {
          case Message("/tr", syn.id, 0, pos: Float) => Swing.onEDT {
            position = (pos + 0.5).toLong
            axis.repaint()
          }
        }
        val start = math.min(position, numFrames - 32768).toInt
//println("PATH = " + inputFile.getAbsolutePath)
        val newMsg    = syn.newMsg(df.name, args = Seq(
          "buf" -> buf.id, "numFrames" -> numFrames.toFloat, "startFrame" -> start.toFloat,
          "diskAmp" -> diskAmp, "resynthAmp" -> resynthAmp)
        )
        val cueMsg    = buf.cueMsg(path = inputFile.getAbsolutePath, startFrame = start, completion = newMsg)
        val allocMsg  = buf.allocMsg(numFrames = 32768, numChannels = numChannels, completion = cueMsg)
        val recvMsg   = df.recvMsg(completion = allocMsg)
        syn.onEnd {
          buf.close(); buf.free()
          resp.remove()
        }
        playing = Some(Playing(syn))
        s ! recvMsg
        resp.add()

      case _ =>
    }

    updateStopPlay()
  }

  private def synthDef(numChannels: Int) = SynthDef("disk_" + numChannels) {
    import ugen._
    val numFr   = "numFrames".ir
    val phasor  = (Phasor.ar(hi = numFr) + "startFrame".ir) % numFr // sucky resetVal doesn't work
    val pTrig   = Impulse.kr(20)
    val disk0   = DiskIn.ar(numChannels, "buf".ir, loop = 1)
    val disk    = Mix.mono(disk0) * "diskAmp".kr(1)
    val pSmp    = A2K.kr(phasor)
    SendTrig.kr(pTrig, id = 0, value = pSmp)

    val pch   = Pitch.kr(disk) \ 0  // XXX TODO
    val piano = pianoFunc(pch)
    val resyn = Mix.mono(verb(piano)) * "resynthAmp".kr(1)

    val sig: GE = Seq(disk, resyn)

    Out.ar(0, sig)
  }

  private def updateStopPlay() {
    val b = playing.isDefined
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

  private lazy val transportStrip = makeButtonStrip(Seq(
    GoToBegin(goToBegin()),
    Stop(stop()),
    Play(play())
  ))

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
//    Mix.tabulate(12) { i =>
//      Ringz.ar(in = exc, freq = freq * (i + 1), attack = Rand(1.0, 3.0), decay = Rand(0.7, 0.9))
//    }
    DC.ar(0)
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