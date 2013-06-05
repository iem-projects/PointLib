package at.iem.point.sh.sketches
package genetic

import scala.collection.immutable.{IndexedSeq => Vec}
import at.iem.point.illism.rhythm.Ladma
import spire.math.Rational
import Fitness._
import reflect.runtime.{universe => ru}
import ru.{typeOf, TypeTag}
import language.existentials
import scala.util.control.NonFatal

object Evaluation {
  val all: Vec[Meta] = Vec(Meta[GlobalEvaluation], Meta[WindowedEvaluation])
}
sealed trait Evaluation extends (Chromosome => Double) {
  def meta: Meta
}

case class GlobalEvaluation(fun   : GlobalFunction = GlobalFunction.Const(),
                            target: GlobalFunction = GlobalFunction.Const(),
                            error : ErrorFunction  = ErrorFunction.Relative)
  extends Evaluation {

  def apply(c: Chromosome): Double = {
    val eval  = fun(c)
    val t     = target(c)
    error(eval, t)
  }

  def meta = Meta[GlobalEvaluation]
}

case class WindowedEvaluation(window: WindowFunction    = WindowFunction.Events(),
                              fun   : LocalFunction     = LocalFunction.LadmaEntropy,
                              target: LocalFunction     = LocalFunction.Const(0.2),
                              error : ErrorFunction     = ErrorFunction.Relative,
                              aggr  : AggregateFunction = AggregateFunction.Mean)
  extends Evaluation {
  
  private val errorT = error.tupled.apply _

  def apply(c: Chromosome): Double = {
    val slices  = window(c)
    val evals   = slices.map(fun   )
    val targets = slices.map(target)
    val errors  = (evals zip targets).map(errorT)
    aggr(errors)
  }

  def meta = Meta[WindowedEvaluation]
}

object WindowFunction {
  val all: Vec[Meta] = Vec(Meta[Events])

  case class Events(size: Int = 5, step: Int = 2) extends WindowFunction {
    require(step >= 1 && size >= step)

    def apply(c: Chromosome): Vec[Slice] = {
      val slices    = slideByEvents(size, step)(c)
      val zipped    = flatWithAccum(c)
      val w1        = math.max(1, zipped.size - size)
      // require(w1 > 0, s"For $seq w1 is $w1")
      val w2        = w1.toDouble
      val m         = slices.map { case (off, idx, slice) =>
        val w       = math.min(1.0, idx.toDouble / w2)
        Slice(slice, idx, off, w)
      }
      m
    }

    def meta = Meta[Events]
  }
}
sealed trait WindowFunction extends (Chromosome => Vec[Slice]) {
  def meta: Meta
}

case class Slice(sq: Sequence, idx: Int, offset: Rational, w: Double)

object GlobalFunction {
  val all: Vec[Meta] = Vec(Meta[Const])

  case class Wrap(local: LocalFunction) extends GlobalFunction {
    def apply(sq: Chromosome): Double = {
      val win = Slice(sq.flattenCells, 0, 0, 0.5)
      local(win)
    }

    def meta = Meta[Wrap]
  }

  case class Const(d: Double = 0.0) extends GlobalFunction {
    def apply(sq: Chromosome): Double = d

    def meta = Meta[Const]
  }
}
sealed trait GlobalFunction extends (Chromosome       => Double) {
  def meta: Meta
}

object LocalFunction {
  val all: Vec[Meta] = Vec(Meta[LadmaEntropy.type], Meta[Const], Meta[Line], Meta[Exp], Meta[ExpExp])

  case object LadmaEntropy extends LocalFunction {
    def apply(win: Slice): Double = Ladma.entropy(win.sq.toCell)
    def meta = Meta[LadmaEntropy.type]
  }

  case class Const(d: Double = 0.0) extends LocalFunction {
    def apply(win: Slice): Double = d
    def meta = Meta[Const]
  }

  case class Line(lo: Double = 0.0, hi: Double = 1.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linlin(0, 1, lo, hi)
    def meta = Meta[Line]
  }

  case class Exp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi)
    def meta = Meta[Exp]
  }

  case class ExpExp(lo: Double = 1.0, hi: Double = 2.0) extends LocalFunction {
    def apply(win: Slice): Double = win.w.linexp(0, 1, lo, hi).linexp(lo, hi, lo, hi)
    def meta = Meta[ExpExp]
  }
}
sealed trait LocalFunction  extends (Slice           => Double) {
  def meta: Meta
}

// sealed trait LocalTarget    extends (Slice           => Double)

object ErrorFunction {
  val all: Vec[Meta] = Vec(Meta[Relative.type])

  case object Relative extends ErrorFunction {
    def apply(eval: Double, target: Double): Double = math.abs(eval - target) / target

    def meta = Meta[Relative.type]
  }
}
sealed trait ErrorFunction  extends ((Double, Double) => Double) {
  def meta: Meta
}

object AggregateFunction {
  val all: Vec[Meta] = Vec(Meta[Mean.type], Meta[RMS.type])

  case object Mean extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = errors.sum / errors.size
  }

  case object RMS extends AggregateFunction {
    def apply(errors: Vec[Double]): Double = math.sqrt(errors.map(x => x * x).sum / errors.size)
  }
}
sealed trait AggregateFunction extends (Vec[Double] => Double)

////////////////

object Meta {
  implicit def apply[A: TypeTag]: Meta = if (isSingleton[A]) MetaCaseObject[A] else MetaCaseClass[A]

  def isSingleton[A: TypeTag]: Boolean = typeOf[A] <:< typeOf[Singleton]
}
sealed trait Meta {
  type A1

  implicit protected def tt: TypeTag[A1]

  lazy val name: String = {
    val t = typeOf[A1]
    val s = t.toString
    val i = s.lastIndexOf('.') + 1
    s.substring(i)
  }

  override def toString = name
}

object MetaCaseClass {
  import reflect.runtime.{currentMirror => cm}

  private def collectDefaults[A](implicit tt: TypeTag[A]): List[Any] = {
    val (im, ts, mApply) = getApplyMethod[A]
    val syms   = mApply.paramss.flatten
    val args   = syms.zipWithIndex.map { case (p, i) =>
      try {
        val mDef = ts.member(ru.newTermName(s"apply$$default$$${i+1}")).asMethod
        im.reflectMethod(mDef)()
      } catch {
        case NonFatal(e) =>
          println(s"For type ${typeOf[A]}, parameter $p at index $i has no default value")
          throw e
      }
    }
    args
  }

  private def getApplyMethod[A: TypeTag]: (ru.InstanceMirror, ru.Type, ru.MethodSymbol) = {
    val clazz  = typeOf[A].typeSymbol.asClass
    val mod    = clazz.companionSymbol.asModule
    val im     = cm.reflect(cm.reflectModule(mod).instance)
    val ts     = im.symbol.typeSignature
    val mApply = ts.member(ru.newTermName("apply")).asMethod
    (im, ts, mApply)
  }

  def apply[A: TypeTag]: MetaCaseClass[A] = {
    val defaults  = collectDefaults[A]
    new MetaCaseClass[A](defaults)
  }
}
case class MetaCaseClass[A](defaults: List[Any])(implicit val tt: TypeTag[A]) extends Meta {
  type A1 = A

  def instance(): A = instance(defaults)

  def instance(args: List[Any]): A = {
    val (im, _, mApply) = MetaCaseClass.getApplyMethod[A]
    im.reflectMethod(mApply)(args: _*).asInstanceOf[A]
  }
}
case class MetaCaseObject[A](implicit val tt: TypeTag[A]) extends Meta {
  type A1 = A

  def instance: A = {
    import reflect.runtime.{currentMirror => cm}
    cm.runtimeClass(typeOf[A].typeSymbol.asClass).asInstanceOf[A]
  }
}