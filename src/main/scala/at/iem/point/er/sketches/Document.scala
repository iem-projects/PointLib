package at.iem.point.er.sketches

import de.sciss.synth.io.AudioFileSpec
import scala.annotation.tailrec
import de.sciss.file._
import de.sciss.model.Change
import de.sciss.model.impl.ModelImpl
import de.sciss.span.Span

object Document {
  sealed trait Update { def document: Document }
  case class PitchesChanged(document: Document, change: Change[PitchAnalysis.Product]) extends Update
  case class OnsetsChanged (document: Document, change: Change[MultiResOnsets       ]) extends Update
}
class Document(val file: File, val fileSpec: AudioFileSpec) extends ModelImpl[Document.Update] {
  def createOutputPath(in: File, tag: String, extension: String): File = {
    val nameInP = in.base

    @tailrec def loop(cnt: Int): File = {
      val fOut = file.parent / s"${nameInP}_$tag${if (cnt == 0) "" else cnt.toString}.$extension"
      if (!fOut.exists) fOut else loop(cnt + 1)
    }
    loop(0)
  }

  private var _pitches: PitchAnalysis.Product = Vector.empty
  def pitches = _pitches
  def pitches_=(seq: PitchAnalysis.Product) {
    val old = _pitches
    _pitches = seq
    // sono.pitchOverlay = seq
    // playerViewOption.foreach(_.pitches = seq)
    // ???
    dispatch(Document.PitchesChanged(this, Change(old, seq)))
  }

  private var _onsets = MultiResOnsets.empty
  def onsets = _onsets
  def onsets_=(seq: MultiResOnsets) {
    val old = _onsets
    _onsets = seq
    // sono.onsetsOverlay = seq
    // playerViewOption.foreach(_.onsets = seq)
    dispatch(Document.OnsetsChanged(this, Change(old, seq)))
  }

  def span        = Span(0, fileSpec.numFrames)
  def sampleRate  = fileSpec.sampleRate
}