package de.sciss.jacop

import JaCoP.constraints.Constraint
import scala.collection.mutable

/** Manages all variables, constraints and global constraints for [[JaCoP]] constraint solver. */
class Model extends JaCoP.core.Store {
  var n = 0

  val constr: mutable.Buffer[Constraint] = new mutable.ListBuffer[Constraint]

  def imposeAllConstraints(): Unit = {
    constr.foreach(impose)
    if (trace) constr.foreach(println)
    constr.clear()
  }
}