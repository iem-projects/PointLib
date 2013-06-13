//package at.iem.point.sh.sketches
//
//import scala.annotation.switch
//
//object TimeSignature {
//  def apply(id: Int): TimeSignature = (id: @switch) match {
//    case Raw    .id => Raw
//    case Rounded.id => Rounded
//    case Decimal.id => Decimal
//  }
//  case object Raw     extends TimeSignature { final val id = 0 }
//  case object Rounded extends TimeSignature { final val id = 1 }
//  case object Decimal extends TimeSignature { final val id = 2 }
//}
//sealed trait TimeSignature { def id: Int }
