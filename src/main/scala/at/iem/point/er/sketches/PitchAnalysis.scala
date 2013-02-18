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

    /** Maximum frequency deviation factor within trajectory. */
    def maxFreqDev: Float

    /** Minimum trajectory duration in milliseconds. */
    def trajMinDur: Float
  }
  final class ConfigBuilder extends ConfigLike {
    var input       = new File("input.aif")

    var minFreq     = 60f
    var maxFreq     = 4000f
    var stepSize    = 256
    var binsPerOct  = 16
    var median      = 10
    var ampThresh   = 0.01f
    var peakThresh  = 0.5f
    var inputGain   = 1f

    var maxFreqDev  = math.pow(2,1.0/10).toFloat
    var trajMinDur  = 30.0f

    def build: Config = Impl(input = input,
      minFreq = minFreq, maxFreq = maxFreq, stepSize = stepSize, binsPerOct = binsPerOct,
      median = median, ampThresh = ampThresh, peakThresh = peakThresh, inputGain = inputGain,
      maxFreqDev = maxFreqDev, trajMinDur = trajMinDur
    )

    private final case class Impl(input: File, minFreq: Float, maxFreq: Float, stepSize: Int, binsPerOct: Int,
                                  ampThresh: Float, peakThresh: Float, median: Int, inputGain: Float,
                                  maxFreqDev: Float, trajMinDur: Float) extends Config {
      override def productPrefix = "Config"
    }
  }
  object Config {
    def apply() = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = b.build
  }
  sealed trait Config extends ConfigLike

  type PayLoad = IIdxSeq[Sample]

  // -----

  protected def defaultConfig: Config = Config()

  protected def create(config: Config, observer: Observer, promise: Promise[PayLoad])
                      (implicit exec: ExecutionContext): Processor[PayLoad, Config] = {
    require(config.stepSize.isPowerOfTwo)
    new Proc(config, observer, promise)
  }

  final case class Sample(start: Long, stop: Long, freq: Float, clarity: Float)

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
        stepSize      = 256,
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
      var trajFreq    = 0f
      var trajFreqSum = 0.0
      var trajClarSum = 0.0
      val trajMinSize = (config.trajMinDur * af.sampleRate / 1000 + 0.5).toInt
      val stepSize    = config.stepSize

      if (verbose) println(f"at ${af.sampleRate/1000}%1.1f kHz, a trajMinDur of ${config.trajMinDur}ms equals $trajMinSize frames" )

      def endTraj() {
        if (trajActive) {
          if (off - trajStart >= trajMinSize) {
            val trajSize  = off - trajStart
            val meanFreq  = (trajFreqSum / trajSize).toFloat  // XXX TODO: should use geometric mean
            val meanClar  = (trajClarSum / trajSize).toFloat
            val start     = trajStart * stepSize
            val stop      = off       * stepSize
            val smp       = Sample(start = start, stop = stop, freq = meanFreq, clarity = meanClar)
            seq += smp
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
            trajStart   = off
            trajFreq    = freq
            trajFreqSum = freq
            trajClarSum = clarity
          }

          if (trajActive) {
            if (hasFreq) {
              val ff = if (freq >= trajFreq) freq / trajFreq else trajFreq / freq
              if (ff <= config.maxFreqDev) { // trajectory still valid
                trajFreqSum += freq
                trajClarSum += clarity

              } else {                // frequency jump, start new traj
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
      seq.result()
    }
  }
}