package at.iem.point.er.sketches

import java.awt.{EventQueue, FileDialog}
import java.io.{File, FilenameFilter}
import de.sciss.synth.io.{AudioFileType, AudioFile}
import de.sciss.synth.Optional
import swing.{Swing, Window, Panel, TextField, Alignment, Label, Component}
import swing.event.EditDone
import scalaswingcontrib.group.GroupPanel
import Swing._

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

  object Setting {
    def float(name: String, unit: Optional[String] = None)(getter: () => Float)(setter: Float => Unit): Setting[Float] = {
      new Impl(name, unit, getter, setter)(_.toFloat)
    }

    def int(name: String, unit: Optional[String] = None)(getter: () => Int)(setter: Int => Unit): Setting[Int] = {
      new Impl(name, unit, getter, setter)(_.toInt)
    }

    def vertical(list: List[Setting[_]]): Panel = new GroupPanel {
      import language.reflectiveCalls
      import language.implicitConversions
      if (list.nonEmpty) {
//        implicit def mkSet(seq: Seq[Group]): Seq[GroupInSequential]     = seq.map(c => c: InSequential)

        // XXX TODO... what a crap. Misdesign in GroupPanel
        type InSequential = InGroup[javax.swing.GroupLayout#SequentialGroup]
        type InParallel   = InGroup[javax.swing.GroupLayout#ParallelGroup]
        implicit def mkPar[A <% InParallel]  (seq: Seq[A]): Seq[InParallel]     = seq.map(c => c: InParallel)
        implicit def mkSer[A <% InSequential](seq: Seq[A]): Seq[InSequential]   = seq.map(c => c: InSequential)

        theHorizontalLayout is Sequential(
          Parallel(Trailing)(list.map(_.label): _*),
          Parallel(list.map(_.input): _*),
          Parallel(list.flatMap(_.unit): _*)
        )
        theVerticalLayout is Sequential(list.map { set =>
          val u = set.unit.toList
          Parallel(Baseline)((set.label :: set.input :: u): _*)
        }: _*)
      }
    }

    private final class Impl[A](name: String, unitName: Option[String],
                                getter: () => A, setter: A => Unit)(convert: String => A)
      extends Setting[A] {

      val label = new Label(name, null, Alignment.Right)
      val input = new TextField(8)
      val unit  = unitName.map(new Label(_))

      def reset() {
        input.text = getter().toString
      }

      reset()
      input.listenTo(input)
      input.reactions += {
        case EditDone(tf) => try {
          setter(value)
        } catch {
          case _: NumberFormatException => reset()
        }
      }

      (label :: input :: unit.toList).foreach { c =>
        c.peer.putClientProperty("JComponent.sizeVariant", "small")
      }

      def value: A = convert(input.text)
      def value_=(v: A) { input.text = v.toString; setter(value) }
    }
  }
  sealed trait Setting[A] {
    def label: Component
    def input: Component
    def unit:  Option[Component]

    var value: A
  }

//  def defer(thunk: => Unit) {
//    if (EventQueue.isDispatchThread) thunk else Swing.onEDT(thunk)
//  }

  object Implicits {
    implicit final class RichWindow(val w: Window) extends AnyVal {
      def placeRightOf(that: Window) {
        w.location = (that.location.x + that.size.width) -> that.location.y
      }
    }
  }
}