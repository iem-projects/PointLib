// source: http://sourceforge.net/projects/jacop-solver/

// this was modified by HHR to get rid of the idiotic global model !!!

/** Package for defining variables, constraints, global constraints and
  * search methods for [[JaCoP]] constraint solver in Scala.
  */
package de.sciss.jacop

/** Solution listener that does not print anything (empty).
  * Used to prohibit printing from search.
  */
class EmptyListener[T <: JaCoP.core.Var] extends JaCoP.search.SimpleSolutionListener[T] {
  //
  //  override def executeAfterSolution(search: JaCoP.search.Search[T], select: JaCoP.search.SelectChoicePoint[T]): Boolean = {
  //    val res = super.executeAfterSolution(search, select)
  //    res
  //  }
}

/** Solution listener that prints solutions of search
  * using user specified functions.
  */
class ScalaSolutionListener[T <: JaCoP.core.Var] extends JaCoP.search.SimpleSolutionListener[T] {

  override def executeAfterSolution(search: JaCoP.search.Search[T], select: JaCoP.search.SelectChoicePoint[T]): Boolean = {
    val res = super.executeAfterSolution(search, select)
    printFunctions.foreach(_.apply())
    res
  }
}
