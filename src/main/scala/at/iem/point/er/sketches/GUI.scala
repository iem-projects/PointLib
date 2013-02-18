package at.iem.point.er.sketches

import java.awt.FileDialog
import java.io.{File, FilenameFilter}
import de.sciss.synth.io.{AudioFileType, AudioFile}

object GUI {
  def openAudioFileDialog(): Option[File] = {
    val dlg = new FileDialog(null: java.awt.Frame, "Open Audio File", FileDialog.LOAD)
    dlg.setFilenameFilter(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = {
        val f = new File(dir, name)
        AudioFile.identify(f) match {
          case Some(_: AudioFileType.CanRead) => true
          case _ => false
        }
      }
    })
    dlg.setVisible(true)
    val parent  = dlg.getDirectory
    val name    = dlg.getFile
    if (parent == null || name == null) return None

    val f = new File(parent, name)
    Some(f)
  }
}