package at.iem.point.eh.sketches

import at.iem.point.illism._
import de.sciss.kollflitz.Ops._

object Markov1 extends App {
  val (histoAbs0, histoMkv0) = improvSnippets.map { i =>
    val s     = loadSnippet(i)
    val ivals = s.notes.map(_.pitch).pairMap(_ to _).map(_.semitones % 12)
    val abs   = ivals.histogram

    val m0 = Map.empty[Int, Map[Int, Int]] withDefaultValue (Map.empty withDefaultValue 0)
    val mk = (m0 /: ivals.sliding(2)) { case (res, Seq(pred, succ)) =>
      val m1 = res(pred)
      val m2 = m1 + (succ -> (m1(succ) + 1))
      res + (pred -> m2)
    }

    (abs, mk)
  } .unzip

  val histoAbs = histoAbs0.reduce { (a, b) =>
    (a /: b) { case (res, (key, value)) => res + (key -> (res(key) + value)) }
  }

  val histoMkv = histoMkv0.reduce { (a, b) =>
    (a /: b) { case (res, (pred, mapB)) =>
      val mapA   = res(pred)
      val newMap = (mapA /: mapB) { case (res1, (succ, count)) =>
        res1 + (succ -> (res1(succ) + count))
      }
      res + (pred -> newMap)
    }
  }

  // (0 until 12).map(histoAbs.apply).zipWithIndex.foreach { case (freq, key) => println(s"$key - $freq") }

  // println(histoMkv)

  val table = (0 until 12).map { s0 =>
    val map = histoMkv(s0)
    val abs = (0 until 12).map(map.apply)
    val num = math.max(1, abs.sum)
    val res = abs.map(_.toDouble / num)
    res
  }

  val tableTxt = table.zipWithIndex.map { case (row, rowIdx) =>
    row.map(c => f"$c%1.2f").mkString(s"$rowIdx & ", " & ", "\\\\")
  } .mkString("\n")
  println((0 until 12).mkString("", " & ", "\\\\\\hline"))
  println(tableTxt)
}
