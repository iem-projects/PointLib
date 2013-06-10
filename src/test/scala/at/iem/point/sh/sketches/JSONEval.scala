package at.iem.point.sh.sketches

import at.iem.point.sh.sketches.genetic.{WindowedEvaluation, Evaluation}
import play.api.libs.json.{JsResult, JsValue, Format, Json}

object JSONEval extends App {
//  //  implicit object EvaluationFormat extends Format[Evaluation] {
//  //    def reads(json: JsValue): JsResult[Evaluation] {
//  //      ???
//  //    }
//  //
//  //    def writes(o: Evaluation): JsValue = {
//  //      ???
//  //    }
//  //  }
//
//  implicit val evalFmt = Json.format[Evaluation]
//
//  val eval: Evaluation = WindowedEvaluation()
//  val forth = Json.toJson(eval)
//  println(Json.prettyPrint(forth))

  sealed trait Foo
  case class Bar(i: Int)
  case class Baz(f: Float)

  implicit val barFmt = Json.format[Bar]
  implicit val bazFmt = Json.format[Baz]
  // implicit val fooFmt = Json.format[Foo]   // "No unapply function found"
}