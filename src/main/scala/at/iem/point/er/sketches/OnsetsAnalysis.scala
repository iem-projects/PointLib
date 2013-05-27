/*
 *  OnsetsAnalysis.scala
 *  (PointLib)
 *
 *  Copyright (c) 2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package at.iem.point.er.sketches

import java.io.File
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth
import concurrent.{Await, duration, blocking}
import duration.Duration
import synth.io.AudioFile
import de.sciss.strugatzki.impl.NonRealtimeProcessor
import language.implicitConversions
import de.sciss.strugatzki.impl.NonRealtimeProcessor.BufferSpec
import annotation.switch
import de.sciss.processor.ProcessorFactory
import de.sciss.processor.impl.ProcessorImpl

object OnsetsAnalysis extends ProcessorFactory.WithDefaults {
  var verbose = false

  object Function {
    case object Power     extends Function { final val id = 0 }
    case object MagSum    extends Function { final val id = 1 }
    case object Complex   extends Function { final val id = 2 }
    case object RComplex  extends Function { final val id = 3 }
    case object Phase     extends Function { final val id = 4 }
    case object WPhase    extends Function { final val id = 5 }
    case object MKL       extends Function { final val id = 6 }

    val seq: IIdxSeq[Function] = Vector(Power, MagSum, Complex, RComplex, Phase, WPhase, MKL)

    def apply(id: Int): Function = (id: @switch) match {
      case Power.id     => Power
      case MagSum.id    => MagSum
      case Complex.id   => Complex
      case RComplex.id  => RComplex
      case Phase.id     => Phase
      case WPhase.id    => WPhase
      case MKL.id       => MKL
    }
  }
  sealed trait Function {
    def id: Int
    def productPrefix: String
    def name: String = productPrefix.toLowerCase
  }

  sealed trait ConfigLike {
    /** Audio input file to analyze. Multiple channels will be mixed together to form a mono signal. */
    def input: File

    /** Detection threshold */
    def thresh: Float

    /** Maximum frequency for the pitch tracker in Hertz. */
    def function: Function

    /** FFT size for the onset detection. Must be a power of two. */
    def fftSize: Int

    /**
     * The FFT overlap factor used to step from vector to vector.
     * This equals fftSize / stepSize, so a value of 2 means
     * the window step is half of the fft size (windows are 50% overlapping).
     */
    def fftOverlap: Int

    /** Release or relax time in seconds. */
    def decay: Float

    /** Noise floor threshold. */
    def noiseFloor: Float

    /** Minimum gap in FFT frames between successive onsets. */
    def minGap: Int

    /** Temporal smoothing in fft frames. */
    def median: Int

    /** Linear gain to apply to input signal before feeding into the onset detection. */
    def inputGain:  Float
  }
  object ConfigBuilder {
    def apply(config: Config): ConfigBuilder = {
      val b = new ConfigBuilder
      b.read(config)
      b
    }
  }
  final class ConfigBuilder extends ConfigLike {
    var input                   = new File("input.aif")

    private var _thresh         = 0.5f
    var function                = Function.Complex: Function
    private var _fftSize        = 512
    private var _fftOverlap     = 2
    private var _decay          = 1.0f
    private var _noiseFloor     = 0.1f
    private var _minGap         = 10
    private var _median         = 11
    private var _inputGain      = 1f

    def thresh: Float = _thresh
    def thresh_=(value: Float) {
      require(value >= 0f /* && value <= 1f */, "Requires thresh >= 0")
      _thresh = value
    }

    def fftOverlap: Int = _fftOverlap
    def fftOverlap_=(value: Int) {
      require(value > 0 && value <= 65536 && value.isPowerOfTwo,
        "Requires 0 < fftOverlap <= 65536, and stepSize being a power of two")
      _fftOverlap = value
    }

    def fftSize: Int = _fftSize
    def fftSize_=(value: Int) {
      require(value >= 16 && value <= 65536 && value.isPowerOfTwo,
        "Requires 16 <= fftSize <= 65536, and fftSize being a power of two")
      _fftSize = value
    }

    def median: Int = _median
    def median_=(value: Int) {
      require (value >= 0 && value <= 8192, "Requires 0 <= median <= 8192")
      _median = value
    }

    def minGap: Int = _minGap
    def minGap_=(value: Int) {
      require (value >= 0 && value <= 8192, "Requires 0 <= minGap <= 8192")
      _minGap = value
    }

    def decay: Float = _decay
    def decay_=(value: Float) {
      require(value >= 0f, "Requires decay >= 0")
      _decay = value
    }

    def noiseFloor: Float = _noiseFloor
    def noiseFloor_=(value: Float) {
      require(value >= 0f, "Requires noiseFloor >= 0")
      _noiseFloor = value
    }

    def inputGain: Float = _inputGain
    def inputGain_=(value: Float) {
      require(value > 0f, "Requires inputGain > 0")
      _inputGain = value
    }

    def build: Config = Impl(input = input,
      thresh = _thresh, function = function, fftSize = _fftSize, fftOverlap = _fftOverlap,
      decay = _decay, noiseFloor = _noiseFloor, minGap = _minGap, median = _median, inputGain = _inputGain
    )

    def read(config: Config) {
      input           = config.input
      _thresh         = config.thresh
      function        = config.function
      _fftSize        = config.fftSize
      _fftOverlap     = config.fftOverlap
      _decay          = config.decay
      _noiseFloor          = config.noiseFloor
      _minGap         = config.minGap
      _median         = config.median
      _inputGain      = config.inputGain
    }

    private final case class Impl(input: File, thresh: Float, function: Function, fftSize: Int, fftOverlap: Int,
                                  decay: Float, noiseFloor: Float, minGap: Int, median: Int, inputGain: Float)
      extends Config {

      override def productPrefix = "Config"
    }
  }
  object Config {
    def default = apply().build
    def apply() = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = b.build
  }
  sealed trait Config extends ConfigLike

  type Product  = IIdxSeq[Long]
  type Repr     = Any

  // -----

  protected def defaultConfig: Config = Config()

  protected def prepare(config: Config): Prepared = new Proc(config)

  private final class Proc(val config: Config)
    extends ProcessorImpl[Product, Any] {

    val companion = OnsetsAnalysis

    def body(): Product = blocking {
      import NonRealtimeProcessor.{RenderConfig, render}
      import synth._
      import ugen._

      val spec              = AudioFile.readSpec(config.input)
      val numChannels       = spec.numChannels
      val stepSize          = config.fftSize / config.fftOverlap
      val fftWinType        = 1 // -1 rect, 0 sine, 1 hann

      val fftBuf = BufferSpec("fft", numFrames = config.fftSize)

      val rCfg: RenderConfig = RenderConfig(
        inFile        = config.input,
        inSpec        = spec,
        numFeatures   = 1,  // onsets
        stepSize      = stepSize,
        buffers       = fftBuf :: Nil,
        progress      = progress(_),
        checkAborted  = () => checkAborted()
      )

      def extract(in0: GE): GE = {
        val gain    = config.inputGain / numChannels.sqrt
        val in      = Mix(in0) * gain
        val chain   = FFT(buf = "fft".kr, in = in, hop = 1.0 / config.fftOverlap, winType = fftWinType)
        val pitch   = Onsets.kr(chain = chain, thresh = config.thresh, fun = config.function.id,
          decay = config.decay, noiseFloor = config.noiseFloor, minGap = config.minGap,
          medianSpan = config.median)
        pitch
      }

      val outFile = Await.result(render(rCfg)(extract), Duration.Inf)

      val af = AudioFile.openRead(outFile)
      try {
        extractOnsets(af, config)

      } finally {
        if (af.isOpen) af.close()
      }
    }

    private def extractOnsets(af: AudioFile, config: Config): IIdxSeq[Long] = {
      val seq         = IIdxSeq.newBuilder[Long]
      val bufSize     = 1024
      val buf         = af.buffer(bufSize)
      val cbuf        = buf(0)

      var off         = 0L
      val numFrames   = af.numFrames
      val stepSize    = config.fftSize / config.fftOverlap

      while (off < numFrames) {
        val chunk = math.min(numFrames - off, bufSize).toInt
        af.read(buf, 0, chunk)
        var i = 0; while(i < chunk) {
          val f = cbuf(i)
          if (f > 0f) {
            seq += off * stepSize
          }
        i += 1; off += 1 }
      }

      val r = seq.result()
//      r.foreach(println)
      r
    }
  }
}