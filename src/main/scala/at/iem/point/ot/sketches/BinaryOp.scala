package at.iem.point.ot.sketches

import play.api.libs.json.{JsResult, JsValue, Format, SealedTraitFormat}
import de.sciss.numbers

sealed trait UnaryOp extends (Double => Double)

case class LinearEnvelope(/* srcLo: Double = 0, srcHi: Double = 1, */ lo: Double = 0, hi: Double = 1) extends UnaryOp {
  override def apply(in: Double): Double = {
    import numbers.Implicits._
    in.linlin(0.0, 1.0, lo, hi)
  }
}
case class ExponentialEnvelope(/* srcLo: Double = 0, srcHi: Double = 1, */ lo: Double = 0.1, hi: Double = 1) extends UnaryOp {
  override def apply(in: Double): Double = {
    import numbers.Implicits._
    in.linexp(0.0, 1.0, lo, hi)
  }
}

case object Reciprocal extends UnaryOp {
  override def apply(x: Double): Double = 1.0 / x
}

case class LinLin(srcLo: Double = 0, srcHi: Double = 1, dstLo: Double = 0, dstHi: Double = 1) extends UnaryOp {
  override def apply(in: Double): Double = {
    import numbers.Implicits._
    in.linlin(srcLo, srcHi, dstLo, dstHi)
  }
}
case class ExpLin(srcLo: Double = 0, srcHi: Double = 1, dstLo: Double = 0.1, dstHi: Double = 1) extends UnaryOp {
  override def apply(in: Double): Double = {
    import numbers.Implicits._
    in.linexp(srcLo, srcHi, dstLo, dstHi)
  }
}

object UnaryOp {
  implicit val format = SealedTraitFormat[UnaryOp]
}

sealed trait BinaryOp extends ((Double, Double) => Double)

case object AbsDif extends BinaryOp {
  override def apply(a: Double, b: Double): Double = {
    import numbers.Implicits._
    a absdif b
  }
}
case object GreaterThan extends BinaryOp {
  override def apply(a: Double, b: Double): Double = if (a > b) 1.0 else 0.0
}
case object LessThan extends BinaryOp {
  override def apply(a: Double, b: Double): Double = if (a < b) 1.0 else 0.0
}
case class ComposeBinaryUnary(first: BinaryOp = AbsDif, andThen: UnaryOp = Reciprocal) extends BinaryOp {
  override def apply(a: Double, b: Double): Double = andThen(first(a, b))
}
case class ComposeUnaryBinary(first: UnaryOp = Reciprocal, andThen: BinaryOp = AbsDif) extends BinaryOp {
  override def apply(a: Double, b: Double): Double = andThen(first(a), first(b))
}

object BinaryOp {
  implicit object format extends Format[BinaryOp] {
    def reads(json: JsValue): JsResult[BinaryOp] = {
      ???
    }

    def writes(op: BinaryOp): JsValue = {
      ???
    }
  }
}