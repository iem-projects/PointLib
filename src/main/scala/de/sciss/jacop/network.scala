package de.sciss.jacop

/** Network specification for networkflow constraint
  *
  * @constructor Creates an empty network
  */
class network extends JaCoP.constraints.netflow.NetworkBuilder {

  import scala.collection.mutable

  val nodes = mutable.Map[node, JaCoP.constraints.netflow.simplex.Node]()

  /** Adds nodes to the network
    *
    * @param n node
    */
  def + (n: node): network = {
    val N = addNode(n.name, n.balance)
    nodes += (n -> N)
    // println("## " + N.name + ", " + N.balance)
    this
  }

  /** Gets a node of the network in network format
    *
    * @param n node
    */
  def apply(n: node): JaCoP.constraints.netflow.simplex.Node = nodes(n)

  /** Creates an arc between two nodes.
    *
    * @param source start node of the arc
    * @param destination end node the arc
    * @param weight weight of this arc for cost calculation
    * @param capacity capacity for the flow on this arc
    */
  def arc(source: node, destination: node, weight: IntVar, capacity: IntVar): Unit = {
    // println(source.name + " -> " + destination.name)
    addArc(nodes(source), nodes(destination), weight, capacity)
  }

  def cost(c: IntVar): Unit = setCostVariable(c)
}

/** Node definition for network for networkflow constraint */
case class node(var name: String, var balance: Int)
