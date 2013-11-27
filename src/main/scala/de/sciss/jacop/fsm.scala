package de.sciss.jacop

/** FSM specification for regular constraint. */
class fsm extends JaCoP.util.fsm.FSM {

  import scala.collection.mutable.ArrayBuffer

  val states = ArrayBuffer.empty[state]

  /** FSM specification for regular constraint.
    *
    * @constructor Creates a new FSM.
    * @param n number of states in this FSM.
    */
  def this(n: Int) = {
    this()
    for (i <- 0 until n) add(new state)
  }

  /** Defines initial state for this FSM.
    *
    * @param s state.
    */
  def init(s: state): Unit = {
    initState = s
    add(s)
  }

  /** Defines a list of final state for this FSM.
    *
    * @param st array of states.
    */
  def addFinalStates(st: Array[state]): Unit = st.foreach(add)

  def add(s: state): fsm = {
    states     += s
    allStates add s
    this
  }

  /** Number of states in this FSM. */
  def length = states.length

  /** Gets the state n of this FSM.
    *
    * @param n index of state.
    * @return n-th state
    */
  def apply(n: Int): state = states(n)
}

/** state specification for FSM for regular constraint.
  *
  * @constructor Creates a new state for FSM.
  */
class state extends JaCoP.util.fsm.FSMState {

  import JaCoP.util.fsm._

  /** Transition of FSM.
    *
    * @param tran values for executing this transition.
    * @param that next state for this transition.
    */
  def -> (tran: IntSet, that: state): Unit = {
    transitions.add(new FSMTransition(tran, that))
  }

  /** Transition of FSM.
    *
    * @param tran integer value for executing this transition.
    * @param that next state for this transition.
    */
  def -> (tran: Int, that: state): Unit = {
    transitions.add(new FSMTransition(new IntSet(tran, tran), that))
  }
}
