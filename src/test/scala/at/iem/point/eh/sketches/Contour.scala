//package at.iem.point.eh.sketches
//
//import at.iem.point.illism._
//import java.awt.geom.GeneralPath
//
//object Contour extends App {
//  val sn          = loadSnippet(improvSnippets(1))
//  val notes       = sn.notes
//
//  val gp = new GeneralPath()
//
//  var n = notes.head
//  gp.moveTo(n.offset, n.pitch.midi)
//  val next = notes.find { n2 =>
//    (n2.offset >= n.offset && n2.offset <= n.stop) && n2.pitch.midi < n.pitch.midi
//  }
//  next match {
//    case Some(n2) => gp.moveTo(n2.offset, n2.pitch.midi)
//    case _ => gp.lineTo(n.stop, n.pitch.midi)
//      val succ = notes.filter { n2 =>
//        n2.offset >= n.stop
//      }
//      if (succ.nonEmpty) {
//
//      }
//  }
//}