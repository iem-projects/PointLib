//package at.iem.point.sh.sketches
//
//import play.api.libs.json.{Reads, JsError, JsResult, Format, JsValue, JsString, JsObject, Json}
//
//object JSONEval extends App {
////  //  implicit object EvaluationFormat extends Format[Evaluation] {
////  //    def reads(json: JsValue): JsResult[Evaluation] {
////  //      ???
////  //    }
////  //
////  //    def writes(o: Evaluation): JsValue = {
////  //      ???
////  //    }
////  //  }
////
////  implicit val evalFmt = Json.format[Evaluation]
////
////  val eval: Evaluation = WindowedEvaluation()
////  val forth = Json.toJson(eval)
////  println(Json.prettyPrint(forth))
//
//  implicit val barFmt = Json.format[Bar]
//  implicit val bazFmt = Json.format[Baz]
//
//  trait SealedTraitFormat[A] {
//    def unapply(value: A): Option[(String, JsValue)] = {
//      ???
//    }
//
//    def apply(`class`: String, data: JsValue): A = {
//      ???
//    }
//
//    // private def
//  }
//
//  object Foo {
//    def unapply(foo: Foo): Option[(String, JsValue)] = {
//      val (prod: Product, sub) = foo match {
//        case b: Bar => (b, Json.toJson(b)(barFmt))
//        case b: Baz => (b, Json.toJson(b)(bazFmt))
//      }
//      Some(prod.productPrefix -> sub)
//    }
//
//    def apply(`class`: String, data: JsValue): Foo = {
//      (`class` match {
//        case "Bar" => Json.fromJson[Bar](data)(barFmt)
//        case "Baz" => Json.fromJson[Baz](data)(bazFmt)
//      }).get
//    }
//  }
//  sealed trait Foo
//  case class Bar(i: Int  ) extends Foo
//  case class Baz(f: Float) extends Foo
//
//  // implicit val fooFmt = Json.format[Foo]
//
//  implicit val fooFmt: Format[Foo] = new Format[Foo] {
//    def reads(json: JsValue): JsResult[Foo] = json match {
//      case JsObject(Seq(("class", JsString(name)), ("data", data))) =>
//        name match {
//          case "Bar"  => Json.fromJson[Bar](data)(barFmt)
//          case "Baz"  => Json.fromJson[Baz](data)(bazFmt)
//          case _      => JsError(s"Unknown class '$name'")
//        }
//
//      case _ => JsError(s"Unexpected JSON value $json")
//    }
//
//    def writes(foo: Foo): JsValue = {
//      val (prod: Product, sub) = foo match {
//        case b: Bar => (b, Json.toJson(b)(barFmt))
//        case b: Baz => (b, Json.toJson(b)(bazFmt))
//      }
//      JsObject(Seq("class" -> JsString(prod.productPrefix), "data" -> sub))
//    }
//  }
//
//  val in: Foo = Bar(33)
//  val js  = Json.toJson(in)
//  println(Json.prettyPrint(js))
//
//  val out = Json.fromJson[Foo](js).getOrElse(sys.error("Oh no!"))
//  assert(in == out)
//
//  //  implicit val treeR: Reads[Foo] =
//  //  __.read[Bar].map(x => x: Foo) orElse
//  //  (
//  //  (__ \ "l").lazyRead(treeR) and (__ \ "r").lazyRead(treeR)
//  //  )(Node.apply _).map(x => x:Tree)
//
//  //  case class Schoko(`class`: String, data: Bar)
//  //  implicit val schokoFmt = Json.format[Schoko]
//  //
//  //  val forth2 = Json.toJson(Schoko("Bar", Bar(33)))
//  //  println(forth2)
//}