package at.iem.point.sh.sketches

import play.api.libs.json.{JsValue, JsString, JsObject, Json}

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

  implicit val barFmt = Json.format[Bar]
  implicit val bazFmt = Json.format[Baz]

  trait SealedTraitCompanion[A] {
    def unapply(value: A): Option[(String, JsValue)] = {
      ???
    }

    def apply(`class`: String, data: JsValue): A = {
      ???
    }
  }

  object Foo {
    def unapply(foo: Foo): Option[(String, JsValue)] = {
      val (prod: Product, sub) = foo match {
        case b: Bar => (b, Json.toJson(b)(barFmt))
        case b: Baz => (b, Json.toJson(b)(bazFmt))
      }
      Some(prod.productPrefix -> sub)
    }

    def apply(`class`: String, data: JsValue): Foo = {
      (`class` match {
        case "Bar" => Json.fromJson[Bar](data)(barFmt)
        case "Baz" => Json.fromJson[Baz](data)(bazFmt)
      }).get
    }
  }
  sealed trait Foo
  case class Bar(i: Int  ) extends Foo
  case class Baz(f: Float) extends Foo

  implicit val fooFmt = Json.format[Foo]

  val in: Foo = Bar(33)
  val js  = Json.toJson(in)
  println(Json.prettyPrint(js))

  val out = Json.fromJson[Foo](js).getOrElse(sys.error("Oh no!"))
  assert(in == out)

  //  case class Schoko(`class`: String, data: Bar)
  //  implicit val schokoFmt = Json.format[Schoko]
  //
  //  val forth2 = Json.toJson(Schoko("Bar", Bar(33)))
  //  println(forth2)
}