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
import collection.immutable.{IndexedSeq => Vec, Iterable => IIterable, Seq => ISeq}
import collection.breakOut

/** Package for defining variables, constraints, global constraints and search
  * methods for [[JaCoP]] constraint solver in Scala.
  */
package object jacop {

  val trace = false

  private var allSolutions  = false

  private var _printFunctions = Vec.empty[() => Unit]

  def printFunctions: Vec[() => Unit] = _printFunctions

  private var labels: Array[DepthFirstSearch[_ <: JaCoP.core.Var]] = null

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
  def alldifferent(xs: IIterable[IntVar])(implicit model: Model): Unit = {
    val c = new Alldiff(xs.toArray[JaCoP.core.IntVar])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Alldistinct]].
    *
    * @param x array of variables to be different.
    */
  def alldistinct(x: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Alldistinct(x.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.GCC]].
    *
    * @param x array of variables.
    * @param y array of counters of differnet values from array x.
    */
  def gcc(x: Array[IntVar], y: Array[IntVar])(implicit model: Model): Unit = {
    val c = new GCC(x.asInstanceOf[Array[JaCoP.core.IntVar]], y.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Sum]].
    *
    * @param res array of variables to be summed up.
    * @param result summation result.
    */
  def sum[A <: JaCoP.core.IntVar](res: IIterable[A], result: IntVar)(implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Sum(res.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Sum]].
    *
    * @param res array of variables to be summed up.
    * @return summation result.
    */
  def sum[A <: JaCoP.core.IntVar](res: IIterable[A])(implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new Sum(res.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.constraints.SumWeight]].
    *
    * @param res array of variables to be summed up.
    * @param w array of weights.
    * @param result summation result.
    */
  def weightedSum[A <: JaCoP.core.IntVar](res: IIterable[A], w: Array[Int], result: IntVar)
                                         (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new SumWeight(res.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], w, result)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.SumWeight]].
    *
    * @param res array of variables to be summed up.
    * @param w array of weights.
    * @return summation result.
    */
  def sum[A <: JaCoP.core.IntVar](res: IIterable[A], w: Array[Int])
                                 (implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new SumWeight(res.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], w, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[JaCoP.constraints.AbsXeqY]].
    *
    * @param x variable for abs operation.
    * @return absolute value result.
    */
  def abs(x: JaCoP.core.IntVar)(implicit model: Model): IntVar = {
    val result = new IntVar()
    val c = new AbsXeqY(x, result)
    if (trace) println(c)
    model.impose(c)
    result
  }

  /** Wrapper for [[JaCoP.constraints.Max]].
    *
    * @param x array of variables where maximum values is to be found.
    * @param mx maxumum value.
    */
  def max[A <: JaCoP.core.IntVar](x: IIterable[A], mx: JaCoP.core.IntVar)
                                 (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Max(x.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], mx)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Min]].
    *
    * @param x array of variables where mnimimum values is to be found.
    * @param mn minimum value.
    */
  def min[A <: JaCoP.core.IntVar](x: IIterable[A], mn: JaCoP.core.IntVar)
                                 (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Min(x.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], mn)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Max]].
    *
    * @param x array of variables where maximum values is to be found.
    * @return max value.
    */
  def max[A <: JaCoP.core.IntVar](x: IIterable[A])(implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new Max(x.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.constraints.Min]].
    *
    * @param x array of variables where minimum values is to be found.
    * @return minimum value.
    */
  def min[A <: JaCoP.core.IntVar](x: IIterable[A])(implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new Min(x.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.constraints.Count]].
    *
    * @param list list of variables to count number of values value.
    * @param count of values value.
    */
  def count[A <: JaCoP.core.IntVar](list: IIterable[A], count: A, value: Int)
                                   (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Count(list.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], count, value)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Count]].
    *
    * @param list list of variables to count number of values value.
    * @return number of values value.
    */
  def count[A <: JaCoP.core.IntVar](list: IIterable[A], value: Int)
                                   (implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new Count(list.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result, value)
    model.constr += c
    println(result)
    result
  }

  /** Wrapper for [[JaCoP.constraints.Values]].
    *
    * @param list list of variables to count number of different values.
    * @param count of different values.
    */
  def values[A <: JaCoP.core.IntVar](list: IIterable[A], count: IntVar)
                                    (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Values(list.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], count)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Values]].
    *
    * @param list list of variables to count number of different values.
    * @return number of different values.
    */
  def values[A <: JaCoP.core.IntVar](list: IIterable[A])(implicit m: ClassTag[A], model: Model): IntVar = {
    val result = new IntVar()
    val c = new Values(list.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], result)
    model.constr += c
    result
  }

  /** Wrapper for [[JaCoP.constraints.Element]].
    *
    * @param index index to select element from list of elements.
    * @param elements array of integers that can be assigned to values.
    * @param value value selected from list of elements.
    */
  def element(index: JaCoP.core.IntVar, elements: Array[Int], value: JaCoP.core.IntVar)(implicit model: Model): Unit = {
    val c = new Element(index, elements, value)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Element]].
    *
    * @param index index to select element from list of elements.
    * @param elements array of integers that can be assigned to values.
    * @param value value selected from list of elements.
    * @param offset value of index offset (shift).
    */
  def element(index: JaCoP.core.IntVar, elements: Array[Int], value: JaCoP.core.IntVar, offset: Int)
             (implicit model: Model): Unit = {
    val c = new Element(index, elements, value, offset)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Element]].
    *
    * @param index index to select element from list of elements.
    * @param elements array of varibales that can be assigned to values.
    * @param value value selected from list of elements.
    */
  def element[A <: JaCoP.core.IntVar](index: JaCoP.core.IntVar, elements: IIterable[A], value: JaCoP.core.IntVar)
                                     (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Element(index, elements.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], value)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Element]].
    *
    * @param index index to select element from list of elements.
    * @param elements array of varibales that can be assigned to values.
    * @param value value selected from list of elements.
    * @param offset value of index offset (shift).
    */
  def element[A <: JaCoP.core.IntVar](index: JaCoP.core.IntVar, elements: IIterable[A], value: JaCoP.core.IntVar,
                                      offset: Int)(implicit m: ClassTag[A], model: Model): Unit = {
    val c = new Element(index, elements.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], value, offset)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Diff2]].
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
    * @param rectangles array of four element vectors representing rectnagles [x, y, lx, ly]
    */
  def diff2(rectangles: Array[Array[IntVar]])(implicit model: Model): Unit = {
    val c = new Diff(rectangles.asInstanceOf[Array[Array[JaCoP.core.IntVar]]])
    if (trace) println(c)
    model.impose(new Diff(rectangles.asInstanceOf[Array[Array[JaCoP.core.IntVar]]]))
  }

  /** Wrapper for [[JaCoP.constraints.Cumulative]].
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
    * @param n array of varibales, which domains define next nodes in the graph.
    */
  def circuit(n: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Circuit(n.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Assignment]].
    *
    * @param x array of varibales.
    * @param y array variables that values are permutation of x.
    */
  def assignment(x: Array[IntVar], y: Array[IntVar])(implicit model: Model): Unit = {
    val c = new Assignment(x.asInstanceOf[Array[JaCoP.core.IntVar]], y.asInstanceOf[Array[JaCoP.core.IntVar]])
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.Among]].
    *
    * @param list array of varibales.
    * @param kSet values to be checked.
    * @param n number of values found.
    */
  def among(list: Array[IntVar], kSet: IntSet, n: IntVar)(implicit model: Model): Unit = {
    val c = new Among(list.asInstanceOf[Array[JaCoP.core.IntVar]], kSet, n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.AmongVar]].
    *
    * @param listX array of varibales.
    * @param listY array of varibales to be checked if their values .
    * @param n number of values found.
    */
  def among(listX: Array[IntVar], listY: Array[IntVar], n: IntVar)(implicit model: Model): Unit = {
    val c = new AmongVar(listX.asInstanceOf[Array[JaCoP.core.IntVar]], listY.asInstanceOf[Array[JaCoP.core.IntVar]], n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.ExtensionalSupportVA]].
    *
    * @param list array of variables.
    * @param tuples array of tuples allowed to be assigned to variables.
    */
  def table[A <: JaCoP.core.IntVar](list: IIterable[A], tuples: Array[Array[Int]])
                                   (implicit m: ClassTag[A], model: Model): Unit = {
    val c = new ExtensionalSupportVA(list.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], tuples)
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
    * @param bin list containing which bin is assigned to an item.
    * @param load list of loads for bins.
    * @param w array of weights for items.
    */
  def binpacking(bin: List[IntVar], load: List[IntVar], w: Array[Int])(implicit model: Model): Unit = {
    val c = new Binpacking(bin.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], load.toArray.asInstanceOf[Array[JaCoP.core.IntVar]], w)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.constraints.regular.Regular]].
    *
    * @param dfa specification of finite state machine using class fsm.
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
    * @param x array of vectors of varibales to be lexicographically ordered.
    */
  def lex(x: Array[Array[IntVar]])(implicit model: Model): Unit = {
    val c = new JaCoP.constraints.Lex(x.asInstanceOf[Array[Array[JaCoP.core.IntVar]]])
    if (trace) println(c)
    model.imposeDecomposition(c)
  }

  /** Wrapper for [[JaCoP.constraints.SoftAlldifferent]].
    *
    * @param xVars array of variables to be constrained to be different.
    * @param costVar measures degree of violation (uses value based violation).
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

  def network_flow(net: JaCoP.constraints.netflow.NetworkBuilder)(implicit model: Model): Unit = {
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

  /** Wrapper for [[JaCoP.constraints.Or]].
    *
    * @param list constraints to be disjunction.
    * @return the constraint that is a a disjunction of constraints.
    */
  def OR(list: IIterable[PrimitiveConstraint])(implicit model: Model): PrimitiveConstraint = {
    val c = new Or(list.toArray)
    list.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[JaCoP.constraints.And]].
    *
    * @param list constraints to be conjunction.
    * @return the constraint that is a a conjunction of constraints.
    */
  def AND(list: PrimitiveConstraint*)(implicit model: Model): PrimitiveConstraint = {
    val c = new And(list.toArray)
    list.foreach(e => model.constr.remove(model.constr.indexOf(e)))
    model.constr += c
    c
  }

  /** Wrapper for [[JaCoP.constraints.And]].
    *
    * @param list constraints to be conjunction.
    * @return the constraint that is a a conjunction of constraints.
    */
  def AND(list: IIterable[PrimitiveConstraint])(implicit model: Model): PrimitiveConstraint = {
    val c = new And(list.toArray)
    list.foreach(e => model.constr.remove(model.constr.indexOf(e)))
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

  /** Wrapper for [[JaCoP.set.constraints.CardAeqX]].
    *
    * @param s constrained set variable.
    * @param n cardinality (IntVar variable).
    */
  def card(s: SetVar, n: JaCoP.core.IntVar)(implicit model: Model): Unit = {
    val c = new CardAeqX(s, n)
    if (trace) println(c)
    model.impose(c)
  }

  /** Wrapper for [[JaCoP.set.constraints.Match]].
    *
    * @param a  a set variable to be matched against list of IntVar.
    * @param list varibales that get values from the set.
    */
  def matching[A <: JaCoP.core.IntVar](a: SetVar, list: IIterable[A])
                                      (implicit m: ClassTag[A], model: Model) {
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

    val label = dfs()
    labels = Array(label)

    _printFunctions = printSolutions.toIndexedSeq
    if (_printFunctions.nonEmpty) {
      label.setSolutionListener(new EmptyListener[A])
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A])
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
                                  (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val label = dfs()
    labels = Array(label)

    _printFunctions = printSolutions.toIndexedSeq
    if (_printFunctions.nonEmpty) {
      // label.setSolutionListener(new EmptyListener[T]);
      label.setPrintInfo(false)
      label.setSolutionListener(new ScalaSolutionListener[A])
    }

    if (timeOut > 0)
      label.setTimeOut(timeOut)

    if (allSolutions)
      label.getSolutionListener.searchAll(true)

    if (maxNumSolutions > 0)
      label.getSolutionListener.setSolutionLimit(maxNumSolutions)

    label.getSolutionListener.recordSolutions(recordSolutions)

    label.labeling(model, select)

  }

  /** Search method that finds all solutions.
    *
    * @param select select method defining variable selection and value assignment methods.
    * @return true if solution found and false otherwise.
    */
  def satisfyAll[A <: JaCoP.core.Var](select: SelectChoicePoint[A], printSolutions: (() => Unit)*)
                                     (implicit m: ClassTag[A], model: Model): Boolean = {

    allSolutions = true

    satisfy(select, printSolutions: _*)
  }


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

    val masterLabel = dfs()

    if (printSolutions.size > 0) {
      masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (maxNumSolutions > 0) masterLabel.respectSolutionLimitInOptimization = true
    if (timeOut         > 0) masterLabel.setTimeOut(timeOut)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs()
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

    _printFunctions = printSolutions.toIndexedSeq
    if (_printFunctions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A])

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
                                     (implicit m: ClassTag[A], model: Model): Boolean = {

    model.imposeAllConstraints()

    val masterLabel = dfs()

    if (printSolutions.size > 0) {
      masterLabel.setSolutionListener(new EmptyListener[A])
      masterLabel.setPrintInfo(false)
    }

    if (timeOut > 0 ) masterLabel.setTimeOut(timeOut)
    if (allSolutions) masterLabel.getSolutionListener.searchAll(true)

    masterLabel.getSolutionListener.recordSolutions(recordSolutions)

    val lastLabel = (masterLabel /: select) { (previousSearch, sel) =>
      val label = dfs()
      previousSearch.addChildSearch(label)
      label.setSelectChoicePoint(sel)

      if (printSolutions.size > 0) {
        label.setSolutionListener(new EmptyListener[A])
        label.setPrintInfo(false)
      }

      if (timeOut > 0 ) label.setTimeOut(timeOut)
      if (allSolutions) label.getSolutionListener.searchAll(true)

      label.getSolutionListener.recordSolutions(recordSolutions)

      label
    }

    _printFunctions = printSolutions.toIndexedSeq
    if (_printFunctions.nonEmpty) {
      lastLabel.setPrintInfo(false)
      lastLabel.setSolutionListener(new ScalaSolutionListener[A])

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
                                        (implicit m: ClassTag[A], model: Model): Boolean = {

    allSolutions = true

    satisfySeq(select, printSolutions: _*)
  }

  /** Depth first search method.
    *
    * @return standard depth first search.
    */
  def dfs[A <: JaCoP.core.Var]()(implicit m: ClassTag[A]): DepthFirstSearch[A] = {
    val label = new DepthFirstSearch[A]

    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[A]())
    if (allSolutions)
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

  /** Defines null variable selection method that is interpreted by JaCoP as input order.
    *
    * @return related variable selection method.
    */
  def inputOrder = null

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
