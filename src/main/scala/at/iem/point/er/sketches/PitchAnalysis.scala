/*
 *  PitchAnalysis.scala
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
import concurrent.{Await, ExecutionContext, Promise, duration, blocking}
import duration.Duration
import synth.io.AudioFile
import de.sciss.strugatzki.{Processor, ProcessorCompanion}
import de.sciss.strugatzki.impl.{NonRealtimeProcessor, ProcessorImpl}
import language.implicitConversions

object PitchAnalysis extends ProcessorCompanion {
  sealed trait ConfigLike {
    /** Audio input file to analyze. Multiple channels will be mixed together to form a mono signal. */
    def input: File

    /** Minimum frequency for the pitch tracker in Hertz. */
    def minFreq: Float

    /** Maximum frequency for the pitch tracker in Hertz. */
    def maxFreq: Float

    /** Pitch tracker time resolution in sample frames. */
    def stepSize: Int

    /** Pitch tracker coarse frequency resolution in bins per octave. */
    def binsPerOct: Int

    /** Pitch tracker minimum amplitude threshold. */
    def ampThresh: Float

    /** Pitch tracker peak amplitude threshold. */
    def peakThresh: Float

    /** Pitch tracker temporal smoothing in control rate samples (sample rate / stepSize). */
    def median: Int

    /** Linear gain to apply to input signal before feeding into pitch tracker. */
    def inputGain:  Float

    /** Maximum frequency spread factor within the whole trajectory. */
    def maxFreqSpread: Float

    /** Maximum frequency slope factor between adjacent samples of the trajectory. */
    def maxFreqSlope: Float

    /** Minimum trajectory duration in milliseconds. */
    def trajMinDur: Float

    /** Order of the polynomial trajectory curve fitting function (0, 1, or 2) */
    def trajFitOrder: Int
  }
  object ConfigBuilder {
    def apply(config: Config): ConfigBuilder = {
      val b = new ConfigBuilder
      b.read(config)
      b
    }
  }
  final class ConfigBuilder extends ConfigLike {
    var input = new File("input.aif")

    private var _minFreq        = 60f
    private var _maxFreq        = 4000f
    private var _stepSize       = 256
    private var _binsPerOct     = 16
    private var _median         = 10
    private var _ampThresh      = 0.01f
    private var _peakThresh     = 0.5f
    private var _inputGain      = 1f

    private var _maxFreqSpread  = math.pow(2,1.0/6).toFloat
    private var _maxFreqSlope   = math.pow(2,1.0/24).toFloat
    private var _trajFitOrder   = 1
    private var _trajMinDur     = 30.0f

    def minFreq: Float = _minFreq
    def minFreq_=(value: Float) {
      require(value >= 0f && value <= 48000f, "Requires 0 <= minFreq <= 48000")
      if (value > _maxFreq) {
        _minFreq  = _maxFreq
        _maxFreq  = value
      } else {
        _minFreq  = value
      }
    }

    def maxFreq: Float = _maxFreq
    def maxFreq_=(value: Float) {
      require(value >= 0f && value <= 48000f, "Requires 0 <= maxFreq <= 48000")
      if (value < _minFreq) {
        _maxFreq  = _minFreq
        _minFreq  = value
      } else {
        _maxFreq  = value
      }
    }

    def stepSize: Int = _stepSize
    def stepSize_=(value: Int) {
      require(value > 0 && value <= 65536 && value.isPowerOfTwo,
        "Requires 0 < stepSize <= 65536, and stepSize being a power of two")
      _stepSize = value
    }

    def binsPerOct: Int = _binsPerOct
    def binsPerOct_=(value: Int) {
      require(value > 0 && value <= 192, "Requires 0 < binsPerOct <= 192")
      _binsPerOct = value
    }

    def median: Int = _median
    def median_=(value: Int) {
      require (value >= 0 && value <= 8192, "Requires 0 <= median <= 8192")
      _median = value
    }

    def ampThresh: Float = _ampThresh
    def ampThresh_=(value: Float) {
      require(value >= 0f, "Requires ampThresh >= 0")
      _ampThresh = value
    }

    def peakThresh: Float = _peakThresh
    def peakThresh_=(value: Float) {
      require(value >= 0f, "Requires peakThresh >= 0")
      _peakThresh = value
    }

    def inputGain: Float = _inputGain
    def inputGain_=(value: Float) {
      require(value > 0f, "Requires inputGain > 0")
      _inputGain = value
    }

    def maxFreqSpread: Float = _maxFreqSpread
    def maxFreqSpread_=(value: Float) {
      require(value >= 1f, "Requires maxFreqSpread >= 1")
      _maxFreqSpread = value
    }

    def maxFreqSlope: Float = _maxFreqSlope
    def maxFreqSlope_=(value: Float) {
      require(value >= 1f, "Requires maxFreqSlope >= 1")
      _maxFreqSlope = value
    }

    def trajFitOrder: Int = _trajFitOrder
    def trajFitOrder(value: Int) {
      require(value >= 0 && value <= 2, "Requires trajFitOrder to be either 0, 1, or 2")
      _trajFitOrder = value
    }

    def trajMinDur: Float = _trajMinDur
    def trajMinDur_=(value: Float) {
      require(value >= 0f && value <= 60000f, "Requires 0 <= trajMinDur <= 60000")
      _trajMinDur = value
    }

    def build: Config = Impl(input = input,
      minFreq = _minFreq, maxFreq = _maxFreq, stepSize = _stepSize, binsPerOct = _binsPerOct,
      median = _median, ampThresh = _ampThresh, peakThresh = _peakThresh, inputGain = _inputGain,
      maxFreqSpread = _maxFreqSpread, maxFreqSlope = _maxFreqSlope, trajFitOrder = _trajFitOrder,
      trajMinDur = _trajMinDur
    )

    def read(config: Config) {
      input           = config.input
      _minFreq        = config.minFreq
      _maxFreq        = config.maxFreq
      _stepSize       = config.stepSize
      _binsPerOct     = config.binsPerOct
      _median         = config.median
      _ampThresh      = config.ampThresh
      _peakThresh     = config.peakThresh
      _inputGain      = config.inputGain
      _maxFreqSpread  = config.maxFreqSpread
      _maxFreqSlope   = config.maxFreqSlope
      _trajFitOrder   = config.trajFitOrder
      _trajMinDur     = config.trajMinDur
    }

    private final case class Impl(input: File, minFreq: Float, maxFreq: Float, stepSize: Int, binsPerOct: Int,
                                  ampThresh: Float, peakThresh: Float, median: Int, inputGain: Float,
                                  maxFreqSpread: Float, maxFreqSlope: Float, trajFitOrder: Int, trajMinDur: Float)
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

  type PayLoad = IIdxSeq[Sample]

  // -----

  protected def defaultConfig: Config = Config()

  protected def create(config: Config, observer: Observer, promise: Promise[PayLoad])
                      (implicit exec: ExecutionContext): Processor[PayLoad, Config] = {
    new Proc(config, observer, promise)
  }

  final case class Sample(start: Long, stop: Long, freq: CurveFitting.Fit, clarity: Float)

  private final class Proc(val config: Config, val observer: Observer, val promise: Promise[PayLoad])
                          (implicit val executionContext: ExecutionContext)
    extends ProcessorImpl[PayLoad, Config] {

    val companion = PitchAnalysis

    def body(): PayLoad = blocking {
      import NonRealtimeProcessor.{RenderConfig, render}
      import synth._
      import ugen._

      val spec              = AudioFile.readSpec(config.input)
      val numChannels       = spec.numChannels
      val sampleRate        = spec.sampleRate

      val rCfg: RenderConfig = RenderConfig(
        inFile        = config.input,
        inSpec        = spec,
        numFeatures   = 2,  // (freq, clarity)
        stepSize      = config.stepSize,
//        blockSize = 64,
        progress      = progress(_),
        checkAborted  = () => checkAborted()
      )

      val execFreq  = sampleRate / rCfg.stepSize

      def extract(in0: GE): GE = {
        val gain  = config.inputGain / numChannels.sqrt
        val in    = Mix(in0) * gain
        val pitch = Pitch.kr(in = in, initFreq = 0,
          minFreq   = config.minFreq,   maxFreq    = config.maxFreq,
          execFreq  = execFreq,         binsPerOct = config.binsPerOct, median  = config.median,
          ampThresh = config.ampThresh, peakThresh = config.peakThresh, clarity = 1)
        pitch
      }

      val outFile = Await.result(render(rCfg)(extract), Duration.Inf)

      val af = AudioFile.openRead(outFile)
      try {
        extractPitches(af, config)

      } finally {
        if (af.isOpen) af.close()
      }
    }

    private def extractPitches(af: AudioFile, config: Config): IIdxSeq[Sample] = {
      val seq         = IIdxSeq.newBuilder[Sample]
      val bufSize     = 1024
      val buf         = af.buffer(bufSize)

      var off         = 0L
      val numFrames   = af.numFrames
      var trajActive  = false
      var trajStart   = 0L
      import CurveFitting.Point
      var traj        = Vector.empty[Point]
      var trajMinFreq = 0.0
      var trajMaxFreq = 0.0
      var trajClarSum = 0.0
      val trajMinSize = math.max(config.trajFitOrder, (config.trajMinDur * af.sampleRate / 1000 + 0.5).toInt)  // control rate
      val stepSize    = config.stepSize

      if (verbose) {
        println(f"pitch file at stepSize $stepSize%d has length $numFrames%d")
        println(f"at ${af.sampleRate/1000}%1.1f kHz, a trajMinDur of ${config.trajMinDur}ms equals $trajMinSize frames" )
        println(f"spread = ${config.maxFreqSpread}%1.3f, slope = ${config.maxFreqSlope}%1.3f")
      }

      def endTraj() {
        if (trajActive) {
          val start     = trajStart * stepSize
          val stop      = off * stepSize
          val trajSize  = off - trajStart // control rate

          if (verbose) {
            println(f"time = ${off / af.sampleRate}%1.2fs (frame = $off%d), freq = ${traj.last.y}%1.1fHz")
          }

          if (trajSize >= trajMinSize) {
            val fit       = CurveFitting.solve(points = traj, order = config.trajFitOrder)
            val meanClar  = (trajClarSum / trajSize).toFloat
            val smp       = Sample(start = start, stop = stop, freq = fit, clarity = meanClar)
            seq += smp

            if (verbose) println(":::: END ::::\n")
          } else {

            if (verbose) println(":::: CANCEL ::::\n")
          }
          trajActive = false

        }
      }

      while (off < numFrames) {
        val chunk = math.min(numFrames - off, bufSize).toInt
        af.read(buf, 0, chunk)
        var i = 0; while(i < chunk) {
          val freq    = buf(0)(i)
          val clarity = buf(1)(i)
          val hasFreq = clarity > 0f

          def beginTraj() {
            trajActive  = true
            trajStart   = off     // control rate
            traj        = Vector(Point(0.0, freq))
            trajMinFreq = freq
            trajMaxFreq = freq
            trajClarSum = clarity

            if (verbose) {
              println(":::: BEGIN ::::")
              println(f"time = ${trajStart / af.sampleRate}%1.2fs (frame = $off%d), freq = $freq%1.1fHz")
            }
          }

          if (trajActive) {
            if (hasFreq) {
              val prev      = traj.last
              val prevOff   = prev.x / stepSize           // control rate
              val prevFreq  = prev.y
              val off0      = (off - trajStart).toDouble  // control rate
              val freqRatio = if (freq < prevFreq) prevFreq / freq else freq / prevFreq
              val freqSlope = math.pow(freqRatio, off0 - prevOff)

              if (freqSlope <= config.maxFreqSlope && {
                val freqSpread  = if (freq < trajMinFreq)
                  trajMaxFreq / freq
                else if (freq < trajMaxFreq)
                  math.max(trajMaxFreq / freq, freq / trajMinFreq)
                else
                  freq / trajMinFreq

                freqSpread <= config.maxFreqSpread
              }) {      // trajectory still valid
                traj       :+= Point(off0 * stepSize, freq)
                trajClarSum += clarity

                if (freq < trajMinFreq) trajMinFreq = freq
                if (freq > trajMaxFreq) trajMaxFreq = freq

              } else {  // frequency jump, start new traj
                endTraj()
                beginTraj()
              }
            } else {
              endTraj()
            }
          } else {
            if (hasFreq) {
              beginTraj()
            }
          }
        i += 1; off += 1 }
      }

      endTraj()
      val r = seq.result()
//      if (verbose) r.foreach(println)
      r
    }
  }
}