package de.sciss.jacop

import JaCoP.constraints._
import JaCoP.set.constraints.{XinA, EinA}

/** Defines a finite domain integer variable and its primitive constraints.
  *
  * @constructor Creates a new finite domain integer variable.
  * @param name variable identifier.
  * @param min minimal value of variable's domain.
  * @param max maximal value of variable's domain.
  */
class IntVar(name: String, min: Int, max: Int)(implicit model: Model)
  extends JaCoP.core.IntVar(model, name, min, max) {

  /** Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable.
    * @param min minimal value of variable's domain.
    * @param max maximal value of variable's domain.
    */
  def this(min: Int, max: Int)(implicit model: Model) = {
    this("_$" + model.n, min, max)
    model.n += 1
  }

  /** Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable with minimal and maximal
    *              values in the domain defined by JaCoP.
    * @param name variable's identifier.
    */
  def this(name: String)(implicit model: Model) = {
    this(name, JaCoP.core.IntDomain.MinInt, JaCoP.core.IntDomain.MaxInt)
    model.n += 1
  }

  /** Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable with minimal and maximal
    *              values in the domain defined by JaCoP.
    */
  def this()(implicit model: Model) = {
    this(JaCoP.core.IntDomain.MinInt, JaCoP.core.IntDomain.MaxInt)
    model.n += 1
  }

  /** Defines a finite domain integer variable.
    *
    * @constructor Create a new finite domain integer variable with the domain defined by IntSet.
    * @param dom variable's domain defined as a set of integers (IntSet).
    */
  def this(dom: IntSet)(implicit model: Model) = {
    this()
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /** Defines a finite domain integer variable.
    *
    * @constructor Create a new finite domain integer variable with the domain
    *              defined by IntSet.
    * @param name variable's identifier.
    * @param dom variable's domain defined as a set of integers (IntSet).
    */
  def this(name: String, dom: IntSet)(implicit model: Model) = {
    this(name)
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /** Defines add constraint between two IntVar.
    *
    * @param that a second parameter for the addition constraint.
    * @return IntVar variable being the result of the addition constraint.
    */
  def + (that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XplusYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines add constraint between IntVar and an integer value.
    *
    * @param that a second integer parameter for the addition constraint.
    * @return IntVar variable being the result of the addition constraint.
    */
  def + (that: Int): IntVar = {
    val result = new IntVar()
    val c = new XplusCeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines subtract constraint between two IntVar.
    *
    * @param that a second parameter for the subtraction constraint.
    * @return IntVar variable being the result of the subtraction constraint.
    */
  def - (that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XplusYeqZ(result, that, this)
    model.constr += c
    result
  }

  /** Defines subtract constraint between IntVar and an integer value.
    *
    * @param that a second integer parameter for the subtraction constraint.
    * @return IntVar variable being the result of the subtraction constraint.
    */
  def - (that: Int): IntVar = {
    val result = new IntVar()
    val c = new XplusCeqZ(result, that, this)
    model.constr += c
    result
  }

  /** Defines multiplication constraint between two IntVar.
    *
    * @param that a second parameter for the multiplication constraint.
    * @return IntVar variable being the result of the multiplication constraint.
    */
  def * (that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XmulYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines multiplication constraint between IntVar and an integer value.
    *
    * @param that a second integer parameter for the multiplication constraint.
    * @return IntVar variable being the result of the multiplication constraint.
    */
  def * (that: Int): IntVar = {
    val result = new IntVar()
    val c = new XmulCeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines integer division constraint between two IntVar.
    *
    * @param that a second parameter for the integer division constraint.
    * @return IntVar variable being the result of the integer division constraint.
    */
  def / (that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XdivYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines constraint for integer reminder from division between two IntVar.
    *
    * @param that a second parameter for integer reminder from division constraint.
    * @return IntVar variable being the result of the integer reminder from division constraint.
    */
  def % (that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XmodYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines exponentiation constraint between two IntVar.
    *
    * @param that exponent for the exponentiation constraint.
    * @return IntVar variable being the result of the exponentiation constraint.
    */
  def pow(that: JaCoP.core.IntVar): IntVar = {
    val result = new IntVar()
    val c = new XexpYeqZ(this, that, result)
    model.constr += c
    result
  }

  /** Defines unary "-" constraint for IntVar.
    *
    * @return the defined constraint.
    */
  def unary_- : IntVar = {
    val result = new IntVar()
    val c = new XplusYeqC(this, result, 0)
    model.constr += c
    result
  }

  /** Defines equation constraint between two IntVar.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XeqY(this, that)
    model.constr += c
    c
  }

  /** Defines equation constraint between IntVar and a integer constant.
    *
    * @param that a second parameter for equation constraint.
    * @return the defined constraint.
    */
  def #= (that: Int): PrimitiveConstraint = {
    val c = new XeqC(this, that)
    model.constr += c
    c
  }

  /** Defines inequality constraint between two IntVar.
    *
    * @param that a second parameter for inequality constraint.
    * @return the defined constraint.
    */
  def #!= (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XneqY(this, that)
    model.constr += c
    c
  }

  /** Defines inequality constraint between IntVar and integer constant.
    *
    * @param that a second parameter for inequality constraint.
    * @return the defined constraint.
    */
  def #!= (that: Int): PrimitiveConstraint = {
    val c = new XneqC(this, that)
    model.constr += c
    c
  }

  /** Defines "less than" constraint between two IntVar.
    *
    * @param that a second parameter for "less than" constraint.
    * @return the defined constraint.
    */
  def #< (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XltY(this, that)
    model.constr += c
    c
  }

  /** Defines "less than" constraint between IntVar and integer constant.
    *
    * @param that a second parameter for "less than" constraint.
    * @return the equation constraint.
    */
  def #< (that: Int): PrimitiveConstraint = {
    val c = new XltC(this, that)
    model.constr += c
    c
  }

  /** Defines "less than or equal" constraint between two IntVar.
    *
    * @param that a second parameter for "less than or equal" constraint.
    * @return the defined constraint.
    */
  def #<= (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XlteqY(this, that)
    model.constr += c
    c
  }

  /** Defines "less than or equal" constraint between IntVar and integer constant.
    *
    * @param that a second parameter for "less than or equal" constraint.
    * @return the equation constraint.
    */
  def #<= (that: Int): PrimitiveConstraint = {
    val c = new XlteqC(this, that)
    model.constr += c
    c
  }

  /** Defines "greater than" constraint between two IntVar.
    *
    * @param that a second parameter for "greater than" constraint.
    * @return the defined constraint.
    */
  def #> (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XgtY(this, that)
    model.constr += c
    c
  }

  /** Defines "greater than" constraint between IntVar and integer constant.
    *
    * @param that a second parameter for "greater than" constraint.
    * @return the equation constraint.
    */
  def #> (that: Int): PrimitiveConstraint = {
    val c = new XgtC(this, that)
    model.constr += c
    c
  }

  /** Defines "greater than or equal" constraint between two IntVar.
    *
    * @param that a second parameter for "greater than or equal" constraint.
    * @return the defined constraint.
    */
  def #>= (that: JaCoP.core.IntVar): PrimitiveConstraint = {
    val c = new XgteqY(this, that)
    model.constr += c
    c
  }

  /** Defines "greater than or equal" constraint between IntVar and integer constant.
    *
    * @param that a second parameter for "greater than or equal" constraint.
    * @return the equation constraint.
    */
  def #>= (that: Int): PrimitiveConstraint = {
    val c = new XgteqC(this, that)
    model.constr += c
    c
  }

  /** Defines constraint on inclusion of a IntVar variable value in a set.
    *
    * @param that set that this variable's value must be included.
    * @return the equation constraint.
    */
  def in(that: SetVar): PrimitiveConstraint = {
    if (min == max) {
      val c = new EinA(min, that)
      model.constr += c
      c
    } else {
      val c = new XinA(this, that)
      model.constr += c
      c
    }
  }
}
