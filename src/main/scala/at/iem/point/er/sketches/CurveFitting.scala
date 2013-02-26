package at.iem.point.er.sketches

import annotation.switch

object CurveFitting {
  private type Points = Traversable[Point]

  final case class Point(x: Double, y: Double)

  sealed trait Fit {
    def order: Int
    def apply(x: Double): Double
  }

  final case class PointFit(a: Double) extends Fit {
    def order = 0
    def apply(x: Double) = a

    override def toString = f"f(x) = $a%1.3f"
  }

  /** Result of a linear polynomial fitting procedure.
    *
    * cf. http://www.efunda.com/math/leastsquares/lstsqr1dcurve.cfm
    *
    * @param a   the linear coefficient in `ax + b`
    * @param b   the constant term in `ax + b`
    */
  final case class LinearFit(a: Double, b: Double) extends Fit {
    def order = 1
    def apply(x: Double): Double = a * x + b

    override def toString = f"f(x) = $a%1.3fx + $b%1.3f"
  }

  /** Result of a quadratic polynomial fitting procedure.
    *
    * cf. http://www.efunda.com/math/leastsquares/lstsqr2dcurve.cfm
    *
    * @param a   the quadratic coefficient in `ax^2 + bx + c`
    * @param b   the linear coefficient in `ax^2 + bx + c`
    * @param c   the constant term in `ax^2 + bx + c`
    * @param err the r-squared error which is 1 - (residual sum of squares / total sum of squares)
    */
  final case class QuadraticFit(a: Double, b: Double, c: Double, err: Double) extends Fit {
    def order = 2

    override def toString = f"f(x) = $a%1.3fx^2 + $b%1.3fx + $c%1.3f (r^2 = $err%1.3f)"

    /** Calculates the dependent variable for a given domain point
      *
      * @param x  the position at which to evaluate the polynomial
      * @return   the fitted point for the given position
      */
    def apply(x: Double) = a * (x*x) + b * x + c
  }

  def solve(points: Points, order: Int): Fit = (order: @switch) match {
    case 0 => solvePoint(points)
    case 1 => solveLinear(points)
    case 2 => solveQuadratic(points)
    case _ => throw new IllegalArgumentException(order.toString)
  }

  /** Fits a given set of points on a zeroth polynomial (point)
   *
   * @param points  the set of measured points
   * @return        point fit
   */
  def solvePoint(points: Points): PointFit = {
    require(points.size >= 1)

    var s00   = 0           // sum of x^0 * y^0 = number of points
    var s01   = 0.0         // sum of x^0 * y^1

    points.foreach { p =>
      s01 += p.y
      s00 += 1
    }

    val a = s01 / s00       // plain mean, right?

    PointFit(a)
  }

  /** Fits a given set of points on a first order polynomial (straight line)
    *
    * @param points  the set of measured points
    * @return        coefficients of a line fit onto the set of points
    */
  def solveLinear(points: Points): LinearFit = {
    require(points.size >= 2)

    var s20   = 0.0         // sum of x^2 * y^0
    var s10   = 0.0         // sum of x^1 * y^0
    var s00   = 0           // sum of x^0 * y^0 = number of points

    var s11   = 0.0         // sum of x^1 * y^1
    var s01   = 0.0         // sum of x^0 * y^1

    points.foreach { p =>
      val x   = p.x
      val y   = p.y
      s10    += x
      val x2  = x * x
      s20    += x2

      s11    += x * y
      s01    += y

      s00    += 1
    }

    val a = (s00 * s11 - s10 * s01) / (s00 * s20 - s10 * s10)
    val b = (s01 * s20 - s10 * s11) / (s20 * s00 - s10 * s10)

    new LinearFit(a = a, b = b)
  }

  /** Fits a given set of points on a second order polynomial (quadratic curve)
   *
   * @param points  the set of measured points
   * @return        coefficients of a quadratic polynomial fit onto the set of points
   */
  def solveQuadratic(points: Points): QuadraticFit = {
    require(points.size >= 3)

    var s40   = 0.0         // sum of x^4 * y^0
    var s30   = 0.0         // sum of x^3 * y^0
    var s20   = 0.0         // sum of x^2 * y^0
    var s10   = 0.0         // sum of x^1 * y^0
    var s00   = 0           // sum of x^0 * y^0 = number of points

    var s21   = 0.0         // sum of x^2 * y^1
    var s11   = 0.0         // sum of x^1 * y^1
    var s01   = 0.0         // sum of x^0 * y^1

    points.foreach { p =>
      val x   = p.x
      val y   = p.y
      s10    += x
      val x2  = x * x
      s20    += x2
      s30    += x2 * x
      s40    += x2 * x2

      s21    += x2 * y
      s11    += x * y
      s01    += y

      s00    += 1
    }

    //a = Da/D
    val a = (s21 * (s20 * s00 - s10 * s10) -
             s11 * (s30 * s00 - s10 * s20) +
             s01 * (s30 * s10 - s20 * s20)) /
            (s40 * (s20 * s00 - s10 * s10) -
             s30 * (s30 * s00 - s10 * s20) +
             s20 * (s30 * s10 - s20 * s20))

    // b = Db/D
    val b = (s40 * (s11 * s00 - s01 * s10) -
             s30 * (s21 * s00 - s01 * s20) +
             s20 * (s21 * s10 - s11 * s20)) /
            (s40 * (s20 * s00 - s10 * s10) -
             s30 * (s30 * s00 - s10 * s20) +
             s20 * (s30 * s10 - s20 * s20))

    //c = Dc/D
    val c = (s40 * (s20 * s01 - s10 * s11) -
             s30 * (s30 * s01 - s10 * s21) +
             s20 * (s30 * s11 - s20 * s21)) /
            (s40 * (s20 * s00 - s10 * s10) -
             s30 * (s30 * s00 - s10 * s20) +
             s20 * (s30 * s10 - s20 * s20))

    val yMean = s01/s00

    var resSum  = 0.0
    var totSum  = 0.0
    points.foreach { p =>
      val x     = p.x
      val y     = p.y
      val x2    = x * x
      val yfit  = a * x2 + b * x + c
      val d1    = y - yfit
      resSum   += d1 * d1
      val d2    = y - yMean
      totSum   += d2 * d2
    }
    val err = 1.0 - resSum / totSum

    new QuadraticFit(a = a, b = b, c = c, err = err)
  }
}