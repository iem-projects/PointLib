package de.sciss.jacop

/** Defines an ordered set of integers and basic operations on these sets.
  *
  * @constructor Create a new ordered empty set of integers.
  */
class IntSet extends JaCoP.core.IntervalDomain {

  /** Defines an ordered set of integers and basic operations on these sets.
    *
    * @constructor Create a new ordered set of integers.
    * @param min minimal value of a set interval.
    * @param max maximal value of a set interval.
    */
  def this(min: Int, max: Int) = {
    this()
    addDom(new JaCoP.core.IntervalDomain(min, max))
  }

  /** Defines an ordered set of integers and basic operations on these sets.
    *
    * @constructor Create a new ordered set containing one element.
    * @param el element of set.
    */
  def this(el: Int) = {
    this()
    addDom(new JaCoP.core.IntervalDomain(el, el))
  }

  /** Set union operation on a set and a set with one value.
    *
    * @param n element of set.
    */
  def + (n: Int) : IntSet =  {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    tmp.unionAdapt(n)
    tmp
  }

  /** Set union operation on two sets.
    *
    * @param that set variable.
    */
  def + (that: IntSet): IntSet = {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    tmp.unionAdapt(that)
    tmp
  }

  /** Set intersection operation on a set and a set with one value.
    *
    * @param n element of set.
    */
  def * (n: Int): IntSet = {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    tmp.intersectAdapt(n,n)
    tmp
  }

  /** Set intersection operation on two sets.
    *
    * @param that set variable.
    */
  def * (that: IntSet): IntSet = {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    tmp.intersectAdapt(that)
    tmp
  }

  /** Set subtraction constraint on a set variable and a set of one value.
    *
    * @param n element of set.
    */
  def \ (n: Int): IntSet = {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    tmp.subtractAdapt(n)
    tmp
  }

  /** Set subtraction  operation on a set and a set with one value.
    *
    * @param that element of set.
    */
  def \ (that: IntSet): IntSet = {
    val tmp = new IntSet
    tmp.unionAdapt(this)
    for (i <- 0 until that.size) {
      tmp.subtractAdapt(that.intervals(i).min, that.intervals(i).max)
    }
    tmp
  }

  /** Set complement operation on a set. */
  def unary_~ : IntSet = {
    val tmp = new IntSet(JaCoP.core.IntDomain.MinInt, JaCoP.core.IntDomain.MaxInt)
    for (i <- 0 until this.size)
      tmp.subtractAdapt(intervals(i).min, intervals(i).max)
    tmp
  }

  /** Produces string representation of a set. */
  override def toString: String =
    if (singleton) s"{$value}" else super.toString
}
