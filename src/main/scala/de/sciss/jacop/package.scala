// source: http://sourceforge.net/projects/jacop-solver/

// this was modified by HHR to get rid of the idiotic global model !!!

package de.sciss

import JaCoP.constraints._
import JaCoP.constraints.knapsack._
import JaCoP.constraints.regular._
import JaCoP.constraints.binpacking._
import JaCoP.constraints.netflow._
import JaCoP.search._
import JaCoP.set.constraints._
import JaCoP.set.search._
import scala.reflect.ClassTag
import collection.immutable.{IndexedSeq => Vec, Iterable => IIterable}
import collection.breakOut

/** Package for defining variables, constraints, global constraints and search
  * methods for [[JaCoP]] constraint solver in Scala.
  */
package object jacop {

  var trace = false

  private var labels = Vec.empty[DepthFirstSearch[_ <: JaCoP.core.Var]]

  /** The maximum number of solutions to be explored.
    * `-1` indicates that there is no limit. */
  var maxNumSolutions: Int = -1

  /** The search time out in seconds. `-1` indicates that there is no time out. */
  var timeOut: Int = -1

  var recordSolutions = false

  // =============== Global constraints ===============

  /** Wrapper for [[JaCoP.constraints.Alldiff]].
    *
    * @param xs array of variables to be different.
    */
  def allDifferent(xs: IntVar*)(implicit model: Model): Unit = {
    val c = new Alldiff(xs.toArray[JaCoP.core.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Alldistinct]].
    *
    * @param xs array of variables to be different.
    */
  def allDistinct(xs: IntVar*)(implicit model: Model): Unit = {
    val c = new Alldistinct(xs.toArray[JaCoP.core.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.GCC]].
    *
    * @param xs array of tuples of variables and their counters
    */
  def gcc(xs: (IntVar, IntVar)*)(implicit model: Model): Unit = {
    val (vars, counters) = xs.unzip
    val c = new GCC(vars.toArray[JaCoP.core.IntVar], counters.toArray[JaCoP.core.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  //  /** Wrapper for [[JaCoP.constraints.Sum]].
  //    *
  //    * @param xs array of variables to be summed up.
  //    * @param result summation result.
  //    */
  //  def assignSum(xs: IIterable[IntVar], result: IntVar)(implicit model: Model): Unit = {
  //    val c = new Sum(xs.toArray[JaCoP.core.IntVar], result)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.Sum]].
    *
    * @param xs variables to be summed up.
    * @return summation result.
    */
  def sum(xs: IntVar*)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new Sum(xs.toArray[JaCoP.core.IntVar], result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[JaCoP.constraints.SumWeight]].
  //    *
  //    * @param xs tuples consisting of variables and their corresponding weights
  //    * @param sum summation result.
  //    */
  //  def assignWeightedSum(xs: IIterable[(IntVar, Int)], sum: IntVar)(implicit model: Model): Unit = {
  //    val (vars, weights) = xs.unzip
  //    val c = new SumWeight(vars.toArray[JaCoP.core.IntVar], weights.toArray[Int], sum)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.SumWeight]].
    *
    * @param xs tuples consisting of variables and their respective weights
    * @return summation result.
    */
  def weightedSum(xs: (IntVar, Int)*)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val (vars, weights) = xs.unzip
    val c = new SumWeight(vars.toArray[JaCoP.core.IntVar], weights.toArray[Int], result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[JaCoP.constraints.AbsXeqY]].
    *
    * @param x variable for abs operation.
    * @return absolute value result.
    */
  def abs(x: IntVar)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new AbsXeqY(x, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  //  /** Wrapper for [[JaCoP.constraints.Max]].
  //    *
  //    * @param xs variables where maximum values is to be found.
  //    * @param mx maximum value.
  //    */
  //  def assignMax(xs: IIterable[IntVar], mx: JaCoP.core.IntVar)(implicit model: Model): Unit = {
  //    val c = new Max(xs.toArray[JaCoP.core.IntVar], mx)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  //  /** Wrapper for [[JaCoP.constraints.Min]].
  //    *
  //    * @param xs array of variables where minimum values is to be found.
  //    * @param mn minimum value.
  //    */
  //  def assignMin(xs: IIterable[IntVar], mn: JaCoP.core.IntVar)(implicit model: Model): Unit = {
  //    val c = new Min(xs.toArray[JaCoP.core.IntVar], mn)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.Max]].
    *
    * @param xs variables where maximum values is to be found.
    * @return max value.
    */
  def max(xs: IntVar*)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new Max(xs.toArray[JaCoP.core.IntVar], result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.constraints.Min]].
    *
    * @param xs variables where minimum values is to be found.
    * @return minimum value.
    */
  def min(xs: IntVar*)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new Min(xs.toArray[JaCoP.core.IntVar], result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[JaCoP.constraints.Count]].
  //    *
  //    * @param xs variables to count number of values value.
  //    * @param count of values value.
  //    */
  //  def assignCount(xs: IIterable[IntVar], value: Int, count: IntVar)(implicit model: Model): Unit = {
  //    val c = new Count(xs.toArray[JaCoP.core.IntVar], count, value)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.Count]].
    *
    * @param xs variables to count number of values value.
    * @return number of values value.
    */
  def count(xs: IIterable[IntVar], value: Int)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new Count(xs.toArray[JaCoP.core.IntVar], result, value)
    model.constr += c
    println(result)
    result
  }

  //  /** Wrapper for [[JaCoP.constraints.Values]].
  //    *
  //    * @param xs variables to count number of different values.
  //    * @param count of different values.
  //    */
  //  def assignNumDistinct(xs: IIterable[IntVar], count: IntVar)(implicit model: Model): Unit = {
  //    val c = new Values(xs.toArray[JaCoP.core.IntVar], count)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.Values]].
    *
    * @param xs variables to count number of different values.
    * @return number of different values.
    */
  def numDistinct(xs: IntVar*)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new Values(xs.toArray[JaCoP.core.IntVar], result)
    model.constr += c
    result
  }

  //  /** Wrapper for [[JaCoP.constraints.Element]].
  //    *
  //    * @param index    index to select element from list of elements.
  //    * @param xs       sequence of integers that can be assigned to values.
  //    * @param value    value selected from list of elements.
  //    */
  //  def assignElementAt(index: JaCoP.core.IntVar, xs: Vec[Int], value: JaCoP.core.IntVar)(implicit model: Model): Unit = {
  //    val c = new Element(index, xs.toArray, value)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  //  /** Wrapper for [[JaCoP.constraints.Element]].
  //    *
  //    * @param index    index to select element from list of elements.
  //    * @param xs       sequence of integers that can be assigned to values.
  //    * @param value    value selected from list of elements.
  //    * @param offset   value of index offset (shift).
  //    */
  //  def assignElementAt(index: JaCoP.core.IntVar, xs: Vec[Int], value: JaCoP.core.IntVar, offset: Int = 0)
  //             (implicit model: Model): Unit = {
  //    val c = new Element(index, xs.toArray, value, offset)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.constraints.Element]].
    *
    * @param index    index to select element from list of elements.
    * @param xs       sequence of integers that can be assigned to values.
    * @param offset   value of index offset (shift).
    * @return         the variable yielding the element at the given index
    */
  def elementAt(index: IntVar, xs: Vec[Int], offset: Int = 0)(implicit model: Model): IntVar = {
    val value = new IntVar()
    val c = new Element(index, xs.toArray, value, offset)
    if (trace) println(c)
    model.impose(c)
    value
  }

  /** Wrapper for [[JaCoP.constraints.Diff2]].
    *
    * XXX TODO: remove arrays, unify sequences
    *
    * @param x coordinate X of rectangle.
    * @param y coordinate Y of rectangle.
    * @param lx length in derection X of rectangle.
    * @param ly length in derection Y of rectangle.
    */
  def diff2(x: Array[IntVar], y: Array[IntVar], lx: Array[IntVar], ly: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Diff(x.asInstanceOf[Array[JaCoP.core.IntVar]], y.asInstanceOf[Array[JaCoP.core.IntVar]],
      lx.asInstanceOf[Array[JaCoP.core.IntVar]], ly.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Diff2]].
    *
    * XXX TODO: remove array
    *
    * @param rectangles array of four element vectors representing rectnagles [x, y, lx, ly]
    */
  def diff2(rectangles: Array[Array[IntVar]])(implicit model: Model): Unit = {
    val c = new Diff(rectangles.asInstanceOf[Array[Array[JaCoP.core.IntVar]]])
    if (trace) println(c)
    model.impose(new Diff(rectangles.asInstanceOf[Array[Array[JaCoP.core.IntVar]]]))
  }

  /** Wrapper for [[JaCoP.constraints.Cumulative]].
    *
    * XXX TODO: remove arrays, unify sequences
    *
    * @param t array of start times of tasks.
    * @param d array of duration of tasks.
    * @param r array of number of resources of tasks.
    * @param limit limit on number of resources used in a schedule.
    */
  def cumulative(t: Array[IntVar], d: Array[IntVar], r: Array[IntVar], limit: IntVar)(implicit model: Model): Unit = {
    val c = new Cumulative(t.asInstanceOf[Array[JaCoP.core.IntVar]],
      d.asInstanceOf[Array[JaCoP.core.IntVar]],
      r.asInstanceOf[Array[JaCoP.core.IntVar]], limit)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Circuit]].
    *
    * @param nodes variables, which domains define next nodes in the graph.
    */
  def circuit(nodes: IntVar*)(implicit model: Model): Unit = {
    val c = new Circuit(nodes.toArray[JaCoP.core.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Assignment]].
    *
    * XXX TODO: remove arrays, unify sequences
    *
    * @param x array of variables.
    * @param y array variables that values are permutation of x.
    */
  def assignment(x: Array[IntVar], y: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Assignment(x.asInstanceOf[Array[JaCoP.core.IntVar]], y.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Among]].
    *
    * XXX TODO: remove array, rename to `assignAmong`, add proper `among`
    *
    * @param list array of variables.
    * @param kSet values to be checked.
    * @param n    number of values found.
    */
  def among(list: Array[IntVar], kSet: IntSet, n: IntVar)(implicit model: Model): Unit = {
    val c = new Among(list.asInstanceOf[Array[JaCoP.core.IntVar]], kSet, n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.AmongVar]].
    *
    * XXX TODO: remove arrays, unify sequences, rename to `assignAmong`, add proper `among`
    *
    * @param listX array of variables.
    * @param listY array of variables to be checked if their values .
    * @param n number of values found.
    */
  def among(listX: Array[IntVar], listY: Array[IntVar], n: IntVar)(implicit model: Model): Unit = {
    val c = new AmongVar(listX.asInstanceOf[Array[JaCoP.core.IntVar]], listY.asInstanceOf[Array[JaCoP.core.IntVar]], n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.ExtensionalSupportVA]].
    *
    * XXX TODO: remove arrays, unify sequences, rename to `assignTable`, add proper `table`
    *
    * @param list   array of variables.
    * @param tuples array of tuples allowed to be assigned to variables.
    */
  def table(list: IIterable[IntVar], tuples: Array[Array[Int]])(implicit model: Model): Unit = {
    val c = new ExtensionalSupportVA(list.toArray[JaCoP.core.IntVar], tuples)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.knapsack.Knapsack]].
    *
    * @param profits array of profite for items.
    * @param weights array of weights for items.
    * @param quantity array of quantities of items.
    * @param knapsackCapacity knapsack capacity.
    * @param knapsackProfit profite when selling items.
    */
  def knapsack(profits: Array[Int], weights: Array[Int], quantity: List[IntVar],
               knapsackCapacity: IntVar, knapsackProfit: IntVar)(implicit model: Model): Unit = {
    val c = new Knapsack(profits, weights, quantity.toArray, knapsackCapacity, knapsackProfit)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for JaCoP.constraints.binpack.Binpack (?).
    *
    * @param bins list containing which tuples of bins, their loads and their weights
    */
  def binPacking(bins: (IntVar, IntVar, Int)*)(implicit model: Model): Unit = {
    val (b, load, w) = bins.unzip3
    val c = new Binpacking(b.toArray[JaCoP.core.IntVar], load.toArray[JaCoP.core.IntVar], w.toArray)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.regular.Regular]].
    *
    * @param dfa  specification of finite state machine using class fsm.
    * @param vars list of variables assigned to fsm nodes.
    */
  def regular(dfa: fsm, vars: IIterable[IntVar])(implicit model: Model): Unit = {
    val c = new Regular(dfa, vars.toArray)
    if (trace) println(c)
    model.impose(c)
  }

  // ================== Decompose constraints

  /** Wrapper for [[JaCoP.constraints.Sequence]].
    *
    * @param list list of variables to be constrained.
    * @param set set of values to be checked.
    * @param q length of the sub-sequence.
    * @param min minimal number of occurrences of values in the sub-sequence.
    * @param max maximal number of occurrences of values in the sub-sequence.
    */
  def sequence(list: Array[IntVar], set: IntSet, q: Int, min: Int, max: Int)(implicit model: Model): Unit = {
    val c = new Sequence(list.asInstanceOf[Array[JaCoP.core.IntVar]], set, q, min, max)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[JaCoP.constraints.Stretch]].
    *
    * @param values a list of values to be assigned to sub-sequences.
    * @param min minimal length of the sub-sequence for each value on position i.
    * @param max maximal length of the sub-sequence for each value on position i.
    * @param x list of variables to be constrained.
    */
  def stretch(values: Array[Int], min: Array[Int], max: Array[Int], x: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Stretch(values, min, max, x.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[JaCoP.constraints.Lex]].
    *
    * @param x array of vectors of variables to be lexicographically ordered.
    */
  def lex(x: Array[Array[IntVar]])(implicit model: Model): Unit = {
    val c = new JaCoP.constraints.Lex(x.asInstanceOf[Array[Array[JaCoP.core.IntVar]]])
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[JaCoP.constraints.SoftAlldifferent]].
    *
    * @param xVars    array of variables to be constrained to be different.
    * @param costVar  measures degree of violation (uses value based violation).
    */
  def softAlldifferent(xVars: Array[IntVar], costVar: IntVar)(implicit model: Model): Unit = {
    val violationMeasure = ViolationMeasure.VALUE_BASED
    val c = new SoftAlldifferent(xVars.asInstanceOf[Array[JaCoP.core.IntVar]], costVar, violationMeasure)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[JaCoP.constraints.SoftGCC]].
    *
    * @param xVars array of variables to be constrained to be different.
    * @param hardLowerBound  lower bound on limits that can not be violated.
    * @param hardUpperBound  upper bound on limits that can not be violated
    * @param countedValue values that are counted.
    * @param softCounters specifies preferred values for counters and can be violated.
    */
  def softGCC(xVars: Array[IntVar], hardLowerBound: Array[Int], hardUpperBound: Array[Int], countedValue: Array[Int], softCounters: Array[IntVar],
              costVar: IntVar)(implicit model: Model): Unit = {
    val violationMeasure = ViolationMeasure.VALUE_BASED
    val c = new SoftGCC(xVars.asInstanceOf[Array[JaCoP.core.IntVar]],
      hardLowerBound,
      hardUpperBound,
      countedValue,
      softCounters.asInstanceOf[Array[JaCoP.core.IntVar]],
      costVar, violationMeasure)
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  def networkFlow(net: JaCoP.constraints.netflow.NetworkBuilder)(implicit model: Model): Unit = {
    val c = new NetworkFlow(net)
    if (trace) println(c)
    model.impose(c)
  }

  // ================== Logical operations on constraints


  /** Wrapper for [[JaCoP.constraints.Or]].
    *
    * @param list constraints to be disjunction.
    * @return the constraint that is a a disjunction of constraints.
    */
  def OR(list: PrimitiveConstraint*)(implicit model: Model): PrimitiveConstraint = {
    val c = new Or(list.toArray)
    list.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[JaCoP.constraints.And]].
    *
    * @param xs constraints to be conjunction.
    * @return the constraint that is a a conjunction of constraints.
    */
  def AND(xs: PrimitiveConstraint*)(implicit model: Model): PrimitiveConstraint = {
    val c = new And(xs.toArray)
    xs.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[JaCoP.constraints.Not]].
    *
    * @param constr constraints to be negated.
    * @return the negated constraint.
    */
  def NOT(constr: PrimitiveConstraint)(implicit model: Model): PrimitiveConstraint = {
    val c = new Not(constr)
    model.constr.remove(model.constr.indexOf(constr))
    model.constr += c
    c
  }

  // =============== Set constraints ===============


  /** Wrapper for [[JaCoP.set.constraints.CardAeqX]].
    *
    * @param s constrained set variable.
    * @return variable defining cardinality of s.
    */
  def card(s: SetVar)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new CardAeqX(s, result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.set.constraints.CardA]].
    *
    * @param s constrained set variable.
    * @param n cardinality.
    */
  def card(s: SetVar, n: Int)(implicit model: Model): Unit = {
    val c = new CardA(s, n)
    if (trace) println(c)
    model.impose(c)
  }

  //  /** Wrapper for [[JaCoP.set.constraints.CardAeqX]].
  //    *
  //    * @param s constrained set variable.
  //    * @param n cardinality (IntVar variable).
  //    */
  //  def assignCard(s: SetVar, n: JaCoP.core.IntVar)(implicit model: Model): Unit = {
  //    val c = new CardAeqX(s, n)
  //    if (trace) println(c)
  //    model.impose(c)
  //  }

  /** Wrapper for [[JaCoP.set.constraints.Match]].
    *
    * @param a    a set variable to be matched against list of IntVar.
    * @param list variables that get values from the set.
    */
  def matching[A <: JaCoP.core.IntVar](a: SetVar, list: IIterable[A])(implicit model: Model) {
    val c = new Match(a, list.toArray)
    if (trace) println(c)
    model.impose(c)
  }

  // =============== Search methods ===================

  /** Minimization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def minimize[A <: JaCoP.core.Var](select: SelectChoicePoint[A], cost: IntVar, printSolutions: (() => Unit)*)
                                   (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val label = dfs[A](all = false)
    labels = Vec(label)

    if (printSolutions.nonEmpty) {
      label.setSolutionListener(new EmptyListener[A])
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A](printSolutions))
    }

    if (maxNumSolutions > 0) {
      label.getSolutionListener.setSolutionLimit(maxNumSolutions)
      label.respectSolutionLimitInOptimization = true
    }

    label.labeling(model, select, cost)
  }


  /** Maximization search method.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def maximize[A <: JaCoP.core.Var](select: SelectChoicePoint[A], cost: IntVar,
                                    printSolutions: (() => Unit)*)(implicit m: ClassTag[A], model: Model): Boolean = {

    val costN = new IntVar("newCost", JaCoP.core.IntDomain.MinInt, JaCoP.core.IntDomain.MaxInt)
    costN #= -cost

    minimize(select, costN, printSolutions: _*)
  }

  /** Search method that finds a solution.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @return true if solution found and false otherwise.
    */
  def satisfy[A <: JaCoP.core.Var](select: SelectChoicePoint[A], printSolutions: (() => Unit)*)
                                  (implicit m: ClassTag[A], model: Model): Boolean =
    satisfyImpl(select, printSolutions, all = false)

  private def satisfyImpl[A <: JaCoP.core.Var](select: SelectChoicePoint[A], printSolutions: Seq[() => Unit],
                                               all: Boolean)
                                              (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val label = dfs[A](all = all)
    labels = Vec(label)

    if (printSolutions.nonEmpty) {
      // label.setSolutionListener(new EmptyListener[T]);
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A](printSolutions))
    }

    val lbList = label.getSolutionListener

    if (timeOut > 0)          label.setTimeOut(timeOut)
    if (all)                  lbList.searchAll(true)
    if (maxNumSolutions > 0)  lbList.setSolutionLimit(maxNumSolutions)
    if (recordSolutions)      lbList.recordSolutions(recordSolutions)

    label.labeling(model, select)
  }

  /** Search method that finds all solutions.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @return true if solution found and false otherwise.
    */
  def satisfyAll[A <: JaCoP.core.Var](select: SelectChoicePoint[A], printSolutions: (() => Unit)*)
                                     (implicit m: ClassTag[A], model: Model): Boolean =
    satisfyImpl(select, printSolutions, all = true)

  /** Minimization method for sequence of search methods (specified by list of select methods).
    *
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def minimizeSeq[A <: JaCoP.core.Var](select: IIterable[SelectChoicePoint[A]], cost: IntVar,
                                       printSolutions: (() => Unit)*)
                                      (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val masterLabel = dfs[A](all = false)

    if (printSolutions.size > 0) {
      masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (maxNumSolutions > 0) masterLabel.respectSolutionLimitInOptimization = true
    if (timeOut         > 0) masterLabel.setTimeOut(timeOut)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs[A](all = false)
      previousSearch.addChildSearch(label)
      label.setSelectChoicePoint(sel)

      if (printSolutions.nonEmpty) {
        label.setSolutionListener(new EmptyListener[A])
        label.setPrintInfo(false)
      }

      if (maxNumSolutions > 0) label.respectSolutionLimitInOptimization = true
      if (timeOut         > 0) label.setTimeOut(timeOut)

      label
    }

    if (printSolutions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A](printSolutions))

      if (maxNumSolutions > 0) {
        lastLabel.getSolutionListener.setSolutionLimit(maxNumSolutions)
        lastLabel.respectSolutionLimitInOptimization = true
      }
    }

    masterLabel.labeling(model, select.head, cost)
  }

  /** Maximization method for sequence of search methods (specified by list of select methods).
    *  
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @param cost Cost variable
    * @return true if solution found and false otherwise.
    */
  def maximizeSeq[A <: JaCoP.core.Var](select: IIterable[SelectChoicePoint[A]], cost: IntVar,
                                       printSolutions: (() => Unit)*)
                                      (implicit m: ClassTag[A], model: Model): Boolean = {

    val costN = new IntVar("newCost", JaCoP.core.IntDomain.MinInt, JaCoP.core.IntDomain.MaxInt)
    costN #= -cost

    minimizeSeq(select, costN, printSolutions: _*)
  }

  /** Search method for finding a solution using a sequence of search methods (specified by list of select methods).
    *
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @return true if solution found and false otherwise.
    */
  def satisfySeq[A <: JaCoP.core.Var](select: IIterable[SelectChoicePoint[A]], printSolutions: (() => Unit)*)
                                     (implicit m: ClassTag[A], model: Model): Boolean =
    satisfySeqImpl(select, printSolutions, all = false)

  private def satisfySeqImpl[A <: JaCoP.core.Var](select: IIterable[SelectChoicePoint[A]],
                                                  printSolutions: Seq[() => Unit], all: Boolean)
                                                 (implicit m: ClassTag[A], model: Model): Boolean = {
    model.imposeAllConstraints()

    val masterLabel = dfs[A](all = all)

    if (printSolutions.size > 0) {
      masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (timeOut > 0 ) masterLabel.setTimeOut(timeOut)
    if (all)          masterLabel.getSolutionListener.searchAll(true)

    masterLabel.getSolutionListener.recordSolutions(recordSolutions)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs[A](all = all)
      previousSearch.addChildSearch(label)
      label.setSelectChoicePoint(sel)

      if (printSolutions.size > 0) {
        label.setSolutionListener(new EmptyListener[A])
        label.setPrintInfo(false)
      }

      val lbList = label.getSolutionListener
      if (timeOut > 0 ) label.setTimeOut(timeOut)
      if (all)          lbList.searchAll(true)

      lbList.recordSolutions(recordSolutions)

      label
    }

    if (printSolutions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A](printSolutions))

      if (maxNumSolutions > 0)
        lastLabel.getSolutionListener.setSolutionLimit(maxNumSolutions)
    }

    lastLabel.getSolutionListener.recordSolutions(recordSolutions)

    masterLabel.labeling(model, select.head)
  }

  /** Search method for finding all solutions using a sequence of search methods (specified by list of select methods).
    *  
    * @param select list of select methods defining variable selection and value assignment methods for sequence of searchs.
    * @return true if solution found and false otherwise.
    */
  def satisfyAllSeq[A <: JaCoP.core.Var](select: IIterable[SelectChoicePoint[A]], printSolutions: (() => Unit)*)
                                        (implicit m: ClassTag[A], model: Model): Boolean =
    satisfySeqImpl(select, printSolutions, all = true)

  /** Depth first search method.
    *
    * @return standard depth first search.
    */
  def dfs[A <: JaCoP.core.Var](all: Boolean): DepthFirstSearch[A] = {
    val label = new DepthFirstSearch[A]

    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[A]())
    if (all)
      label.getSolutionListener.searchAll(true)

    label
  }

  /** Defines list of variables, their selection method for search and value selection
    *  
    * @return select method for search.
    */
  def search[A <: JaCoP.core.Var](vars: IIterable[A], heuristic: ComparatorVariable[A], indom: Indomain[A])
                                 (implicit m: ClassTag[A]): SelectChoicePoint[A] =
    new SimpleSelect[A](vars.toArray, heuristic, indom)

  /**
   * Defines list of variables, their selection method for sequential search and value selection
   *
   * @return select method for search.
   */
  def searchVector[A <: JaCoP.core.Var](vars: Vec[Vec[A]], heuristic: ComparatorVariable[A],
                                        indom: Indomain[A])(implicit m: ClassTag[A]): SelectChoicePoint[A] = {
    val varsArray: Array[Array[A]] = vars.map(_.toArray)(breakOut)

    new SimpleMatrixSelect[A](varsArray, heuristic, indom)
  }

  /** Defines list of variables, their selection method for split search and value selection
    *
    * @return select method for search.
    */
  def searchSplit[A <: JaCoP.core.IntVar](vars: IIterable[A], heuristic: ComparatorVariable[A])
                                         (implicit m: ClassTag[A]): SelectChoicePoint[A] =
    new SplitSelect[A](vars.toArray, heuristic, new IndomainMiddle[A]())

  def statistics(): String = {
    var nodes       = 0
    var decisions   = 0
    var wrong       = 0
    var backtracks  = 0
    var depth       = 0
    var solutions   = 0

    for (label <- labels) {
      nodes       += label.getNodes
      decisions   += label.getDecisions
      wrong       += label.getWrongDecisions
      backtracks  += label.getBacktracks
      depth       += label.getMaximumDepth
      solutions    = label.getSolutionListener.solutionsNo()
    }
    
    "Search statistics:\n==================" +
      "\n Search nodes           : " + nodes +
      "\n Search decisions       : " + decisions +
      "\n Wrong search decisions : " + wrong +
      "\n Search backtracks      : " + backtracks +
      "\n Max search depth       : " + depth +
      "\n Number solutions       : " + solutions
  }
  
  def printStatistics(): Unit = {
    println()
    println(statistics())
  }

  //  /** Defines null variable selection method that is interpreted by JaCoP as input order.
  //    *
  //    * @return related variable selection method.
  //    */
  //  def inputOrder = null

  // ---- Generic (IntVar and BooleanVar) ----

  /** Wrapper for [[JaCoP.search.SmallestDomain]].
    *
    * @return related variable selection method.
    */
  def firstFail[A <: JaCoP.core.Var] = new SmallestDomain[A]

  /** Wrapper for [[JaCoP.search.MostConstrainedStatic]].
    *
    * @return related variable selection method.
    */
  def mostConstrained[A <: JaCoP.core.Var] = new MostConstrainedStatic[A]

  /** Wrapper for [[JaCoP.search.LargestDomain]].
    *
    * @return related variable selection method.
    */
  def antiFirstFail[A <: JaCoP.core.Var] = new LargestDomain[A]

  // ---- IntVar specific ----

  /** Wrapper for [[JaCoP.search.SmallestMin]].
    *
    * @return related variable selection method.
    */
  def smallestMin[A <: JaCoP.core.IntVar] = new SmallestMin[A]

  /** Wrapper for [[JaCoP.search.SmallestMin]].
    *
    * @return related variable selection method.
    */
  def smallest[A <: JaCoP.core.IntVar] = new SmallestMin[A]

  /** Wrapper for [[JaCoP.search.LargestMax]].
    *
    * @return related variable selection method.
    */
  def largest[A <: JaCoP.core.IntVar] = new LargestMax[A]

  /** Wrapper for [[JaCoP.search.MaxRegret]].
    *
    * @return related variable selection method.
    */
  def maxRegret[A <: JaCoP.core.IntVar] = new MaxRegret[A]

  /** Wrapper for [[JaCoP.search.IndomainMin]].
    *
    * @return related variable selection method.
    */
  def indomainMin[A <: JaCoP.core.IntVar] = new IndomainMin[A]

  /** Wrapper for [[JaCoP.search.IndomainMax]].
    *
    * @return related variable selection method.
    */
  def indomainMax[A <: JaCoP.core.IntVar] = new IndomainMax[A]

  /** Wrapper for [[JaCoP.search.IndomainMiddle]].
    *
    * @return related variable selection method.
    */
  def indomainMiddle[A <: JaCoP.core.IntVar] = new IndomainMiddle[A]

  /** Wrapper for [[JaCoP.search.IndomainMedian]].
    *
    * @return related variable selection method.
    */
  def indomainMedian[A <: JaCoP.core.IntVar] = new IndomainMedian[A]

  /** Wrapper for [[JaCoP.search.IndomainRandom]].
    *
    * @return related variable selection method.
    */
  def indomainRandom[A <: JaCoP.core.IntVar] = new IndomainRandom[A]

  // ---- Set specific ----

  /** Wrapper for [[JaCoP.set.search.MinCardDiff]].
    *
    * @return related variable selection method.
    */
  def firstFailSet[A <: JaCoP.set.core.SetVar] = new MinCardDiff[A]

  /** Wrapper for [[JaCoP.search.MostConstrainedStatic]].
    *
    * @return related variable selection method.
    */
  def mostConstrainedSet[A <: JaCoP.set.core.SetVar] = new MostConstrainedStatic[A]

  /** Currently equivalent to `minGLBCard`.
    *
    * @return related variable selection method.
    */
  def smallestSet[A <: JaCoP.set.core.SetVar] = minGLBCard[A]

  /** Wrapper for [[JaCoP.set.search.MinGlbCard]].
    *
    * @return related variable selection method.
    */
  def minGLBCard[A <: JaCoP.set.core.SetVar] = new MinGlbCard[A]

  /** Wrapper for [[JaCoP.set.search.MinLubCard]].
    *
    * @return related variable selection method.
    */
  def minLUBCard[A <: JaCoP.set.core.SetVar] = new MinLubCard[A]

  /** Wrapper for [[JaCoP.set.search.MaxCardDiff]].
    *
    * @return related variable selection method.
    */
  def antiFirstFailSet[A <: JaCoP.set.core.SetVar] = new MaxCardDiff[A]

  /** Wrapper for [[JaCoP.set.search.IndomainSetMin]].
    *
    * @return related indomain method.
    */
  def indomainMinSet[A <: JaCoP.set.core.SetVar] = new IndomainSetMin[A]

  /** Wrapper for [[JaCoP.set.search.IndomainSetMax]].
    *
    * @return related indomain method.
    */
  def indomainMaxSet[A <: JaCoP.set.core.SetVar] = new IndomainSetMax[A]

  /** Wrapper for [[JaCoP.set.search.IndomainSetRandom]].
    *
    * @return related indomain method.
    */
  def indomainRandomSet[A <: JaCoP.set.core.SetVar] = new IndomainSetRandom[A]
}
