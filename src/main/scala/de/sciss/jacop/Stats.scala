package de.sciss.jacop

case class Stats(nodes: Int, decisions: Int, wrong: Int, backtracks: Int, depth: Int, solutions: Int) {
  override def toString = "Search statistics:\n==================" +
    "\n Search nodes           : " + nodes +
    "\n Search decisions       : " + decisions +
    "\n Wrong search decisions : " + wrong +
    "\n Search backtracks      : " + backtracks +
    "\n Max search depth       : " + depth +
    "\n Number solutions       : " + solutions
}