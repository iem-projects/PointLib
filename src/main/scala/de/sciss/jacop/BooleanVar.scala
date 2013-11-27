package de.sciss.jacop

import JaCoP.constraints._

/** Defines a boolean variable and its primitive constraints.
  *
  * @constructor Creates a new boolean variable.
  * @param name variable's identifier.
  * @param min1 minimal value for variable's domain.
  * @param max1 maximal value for variable's domain.
  */
class BooleanVar private[jacop](name: String, min1: Int, max1: Int)(implicit model: Model)
  extends JaCoP.core.BooleanVar(model, name, min1, max1) {

  /** Defines a boolean variable with {0..1} domain.
    *
    * @constructor Creates a new boolean variable.
    * @param name variable's identifier.
    */
  def this(name: String)(implicit model: Model) = {
    this(name, 0, 1)
    model.n += 1
  }

  /** Define an anonymous boolean variable with {0..1} domain.
    *
    * @constructor Creates a new boolean variable.
    */
  def this()(implicit model: Model) = {
    this("_$" + model.n, 0, 1)
    model.n += 1
  }

   /* Defines an anonymous boolean variable.
    *
    * @constructor Creates a new boolean variable.
    * @param l minimal value for variable's domain.
    * @param r maximal value for variable's domain.
    */
  private[jacop] def this(l: Int, r: Int)(implicit model: Model) = {
    this("_$" + model.n, l, r)
    model.n += 1
  }

  /** Defines equation constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: JaCoP.core.BooleanVar /* IntVar */): PrimitiveConstraint = {
    val c = new JaCoP.constraints.XeqY(this, that)
    model.constr += c
    c
  }

  /** Defines equation constraint a BoolVar and a integer value.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: Boolean /* Int */): PrimitiveConstraint = {
    val b = if (that) 1 else 0
    val c = new XeqC(this, b)
    model.constr += c
    c
  }

  /** Defines logical and (conjunction) constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def /\ (that: JaCoP.core.BooleanVar /* IntVar */): BooleanVar = {
    val result = new BooleanVar()
    val parameters = Array[JaCoP.core.IntVar](this, that)
    val c = new JaCoP.constraints.AndBool(parameters, result)
    model.constr += c
    result
  }

  /** Defines logical or (disjunction) constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def \/ (that: JaCoP.core.BooleanVar /* IntVar */): BooleanVar = {
    val result = new BooleanVar()
    val parameters = Array[JaCoP.core.IntVar](this, that)
    val c = new JaCoP.constraints.OrBool(parameters, result)
    model.constr += c
    result
  }

  /** Defines logical exclusive or constraint between two BoolVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def xor(that: JaCoP.core.BooleanVar /* IntVar */): BooleanVar = {
    val result = new BooleanVar()
    val c = new JaCoP.constraints.XorBool(this, that, result)
    model.constr += c
    result
  }

  /** Defines logical negation constraint for BoolVar.
    *
    * @return boolean variable that is the result for this constraint.
    */
  def unary_~ : BooleanVar = {
    val result = new BooleanVar()
    val c = new XplusYeqC(this, result, 1)
    model.constr += c
    result
  }

  /** Defines an implication constraint.
    *
    * Note: this assumes that the `thenConstr` posts to the model. The method then
    * removes that posted constraint and replaced it by an amended version.
    * XXX TODO: this is ugly. A better solution would be to have `thenConstr` be
    * a call-by-name parameter and push a temporary model instead?
    *
    * @param thenConstr a primitive constraint that will hold if this variable is 1.
    * @return the defined constraint.
    */
  def -> (thenConstr: PrimitiveConstraint): Constraint = {
    val c: Constraint = new IfThen(new XeqC(this, 1), thenConstr)
    model.constr.remove(model.constr.length - 1)
    model.constr += c
    c
  }

  /** Defines a reified constraint.
    *
    * Note: this assumes that the `thenConstr` posts to the model. The method then
    * removes that posted constraint and replaced it by an amended version.
    * XXX TODO: this is ugly. A better solution would be to have `thenConstr` be
    * a call-by-name parameter and push a temporary model instead?
    *
    * @param reifC a primitive constraint that is used in reification.
    * @return the defined constraint.
    */
  def <=> (reifC: PrimitiveConstraint): Constraint = {
    val c: Constraint = new Reified(reifC, this)
    model.constr.remove(model.constr.length - 1)
    model.constr += c
    c
  }
}
