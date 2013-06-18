//package at.iem.point.sh.sketches
//
//import play.api.libs.json._
//import at.iem.point.sh.sketches.genetic.Evaluation
//
//object JSONTest extends App {
//  case class EucalyptusTree(col: Int, row: Int)
//
//  object EucalyptusTree {
//    implicit val fmt = Json.format[EucalyptusTree]
//  }
//
//  case class Koala(name: String, home: EucalyptusTree)
//
//  object Koala {
//    implicit val fmt = Json.format[Koala]
//  }
//
//  val kaylee = Koala("kaylee", EucalyptusTree(10, 23))
//
//  val forth = Json.toJson(kaylee)
//  println(Json.prettyPrint(forth))
//
//  val back = Json.fromJson[Koala](forth)
//
//  //    Json.obj(
//  //      "name" -> "kaylee",
//  //      "home" -> Json.obj(
//  //        "col" -> 10,
//  //        "row" -> 23
//  //      )
//  //    )
//
//  println(s"Back: $back")
//
//  // ok, that one needs more work (`unapply`)
//  // implicit val evalFmt = Json.format[Evaluation]
//}