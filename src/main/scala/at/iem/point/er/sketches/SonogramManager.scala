//package at.iem.point.er.sketches
//
//import java.io.{File, IOException}
//import de.sciss.sonogram.{SonogramOverviewManager, SonogramFileSpec, SonogramSpec, SonogramOverview}
//import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec, AudioFile}
//import math._
//
//class SonogramManager extends SonogramOverviewManager {
//  override def fromPath(path: File): SonogramOverview = {
//    val af          = AudioFile.openRead(path)
//    val afDescr     = af.spec
//    af.close()    // render loop will re-open it if necessary...
//    val sampleRate  = afDescr.sampleRate
//    val stepSize    = max(64, (sampleRate * 0.0116 + 0.5).toInt) // 11.6ms spacing
//    val sonoSpec    = SonogramSpec(sampleRate, 32, min(16384, sampleRate / 2).toFloat, 24,
//        (stepSize / sampleRate * 1000).toFloat, 4096, stepSize)
//    val decim       = List(1, 6, 6, 6, 6)
//    val fileSpec    = new SonogramFileSpec(sonoSpec, path.lastModified, path,
//      afDescr.numFrames, afDescr.numChannels, sampleRate, decim)
//    // val cachePath     = fileCache.createCacheFileName( path )
//    val cachePath = createCacheFileName(path)
//
//    val d = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, afDescr.numChannels, afDescr.sampleRate)
//    val decimAF = AudioFile.openWrite(cachePath, d) // XXX eventually should use shared buffer!!
//
//    val so = new SonogramOverview(mgr, fileSpec, decimAF)
//    // render overview if necessary
//    queue(so)
//    so
//  }
//}