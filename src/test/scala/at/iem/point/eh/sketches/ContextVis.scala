package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.kollflitz.Ops._
import de.sciss.contextsnake.ContextTree
import de.sciss.file._
import java.io.FileOutputStream

object ContextVis extends App {
  val inp = improvSnippets.take(1)

  // use only one file to make the tree smaller
  val allIvals = inp.flatMap { i =>
    val s = loadSnippet(i)
    val ivals = s.notes.map(_.pitch).pairMap(_ to _).map(_.semitones % 12)
    ivals
  }

  val tree = ContextTree(allIvals: _*)

  //  val dot = tree.toDOT(tailEdges = false)
  //  val dir = basePath / "new_plots"
  //  require(dir.isDirectory)
  //  val f   = dir / "improv_ivals.dot"
  //  val fos = new FileOutputStream(f)
  //  fos.write(dot.getBytes("UTF-8"))
  //  fos.close()

  def explore(init: Vector[Int]): String = {
    val sb = exploreStart()
    exploreMain(init, drop = 0, sb = sb)
    exploreStop(sb)
  }

  def exploreStart(): StringBuffer = {
    val sb = new StringBuffer()
    sb.append("digraph suffixes {\n")
    sb.append( s"""  node [margin=0.05 fontcolor=black fontsize=16 width=0.125 shape=circle style=filled fillcolor=lightgray];\n""")
    sb
  }

  def exploreStop(sb: StringBuffer): String = {
    sb.append("}\n")
    sb.toString
  }

  def exploreMain(init: Vector[Int], drop: Int, sb: StringBuffer): Unit = {
    def iter(body: Vector[Int]): Unit = {
      val snake   = tree.snake(body.drop(drop))
      val succ    = snake.successors.toVector.histogram.toVector.sorted
      val isLeaf  = succ.size <= 1
      val id      = body.mkString("_")
      sb.append(s"""  n$id [label="${body.last}"];\n""")
      succ.foreach { case (suf, count) =>
        val body1 = body :+ suf
        val id1   = body1.mkString("_")
        sb.append( s"""  n$id -> n$id1;\n""")
      }
      succ.foreach { case (suf, count) =>
        val body1 = body :+ suf
        val id1   = body1.mkString("_")
        if (isLeaf) {
          sb.append(s"""  n$id1 [label="$suf"];\n""")
          sb.append(s"""  n$id1 [shape=point];\n""")
        } else {
          iter(body1)
        }
      }
    }
    iter(init)
  }

  val dir = basePath / "new_plots"
  require(dir.isDirectory)

  def mkFile(init: Vector[Int]): Unit = {
    val f   = dir / s"improv${inp.mkString("_")}_context${init.mkString("_")}.dot"
    val dot = explore(init)
    val fos = new FileOutputStream(f)
    fos.write(dot.getBytes("UTF-8"))
    fos.close()
  }

  // case class Step(body: Vector[Int], drop: Int, fillColor: String, fontColor: String)

  def mkFile1(init: Vector[Int], process: Seq[(Int, Vector[Int])]): Unit = {
    val f   = dir / s"improv${inp.mkString("_")}_context${init.mkString("_")}p.dot"
    val sb  = exploreStart()
    ((init, 0) /: process) { case ((body, drop), (nextDrop, grow)) =>
      exploreMain(body, drop, sb)
      val newBody = body ++ grow
      (newBody, drop + nextDrop)
    }
    val dot = exploreStop(sb)
    val fos = new FileOutputStream(f)
    fos.write(dot.getBytes("UTF-8"))
    fos.close()
  }

  //  mkFile(Vector(3))
  //  mkFile(Vector(5, 5))
  //  mkFile(Vector(5, 8))

  mkFile1(Vector(3), Seq(1 -> Vector(5, 5), 2 -> Vector(8)))
}