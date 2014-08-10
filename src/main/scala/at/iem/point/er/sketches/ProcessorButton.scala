package at.iem.point.er.sketches

import scala.swing.{Swing, Button}
import scala.swing.event.{Event, ButtonClicked}
import de.sciss.processor.Processor
import scala.concurrent.ExecutionContext
import scala.swing.Swing.EmptyIcon
import java.awt.EventQueue
import scala.util.Try

object ProcessorButton {
  case class Completed[Product, Repr](button: ProcessorButton[Product, Repr], result: Try[Product]) extends Event

  def apply[Product, Repr](text0: String)(prepare: => Repr with Processor[Product, Repr] with Processor.Prepared)
                                         (action: PartialFunction[Try[Product], Unit])
                                         (implicit context: ExecutionContext): ProcessorButton[Product, Repr] = {
    val b = new ProcessorButton[Product, Repr](text0)(prepare)
    b.listenTo(b)
    val fun = action.lift
    b.reactions += {
      case Completed(_, res: Try[Product]) => fun(res)
    }
    b
  }
}
class ProcessorButton[Product, Repr]
  (text0: String)(prepare: => Repr with Processor[Product, Repr] with Processor.Prepared)
  (implicit context: ExecutionContext)
  extends Button(text0) {

  import ProcessorButton._

  var proc              = Option.empty[Repr with Processor[Product, Repr]]
  private val progIcon  = new ProgressIcon(33)

  private def defer(code: => Unit): Unit =
    if (EventQueue.isDispatchThread) code else Swing.onEDT(code)

  // protected def prepare(): Option[Repr with Processor.Prepared]

  listenTo(this)
  reactions += {
    case ButtonClicked(_) =>
      proc.fold(tryStart())(_.abort())
  }

  preferredSize = {
    setProgressContent()
    val dim1     = preferredSize
    setNormalContent()
    val dim2    = preferredSize
    dim1.width  = math.max(dim1.width , dim2.width )
    dim1.height = math.max(dim1.height, dim2.height)
    dim1
  }

  private def setProgressContent(): Unit = {
    text            = "\u2716"  // 'X'
    progIcon.value  = 0
    icon            = progIcon
  }

  private def setNormalContent(): Unit = {
    icon  = EmptyIcon
    text  = text0
  }

  private def tryStart(): Unit = {
    val prep = Try(prepare).toOption // ()
    proc = prep
    prep.foreach { (p: Processor[Product, Repr] with Processor.Prepared) =>
      p.addListener {
        case prog @ Processor.Progress(_, _) => defer {
          progIcon.value = prog.toInt
          repaint()
        }
      }
      p.start()
      setProgressContent()
      p.onComplete {
        case res => defer {
          setNormalContent()
          proc  = None
          publish(Completed(this, res))
        }
      }
    }
  }
}