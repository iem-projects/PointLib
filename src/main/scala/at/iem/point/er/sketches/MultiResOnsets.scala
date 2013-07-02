package at.iem.point.er.sketches

import scala.annotation.tailrec

object MultiResOnsets {
  case class Config(maxDrift: Int = 4)

  def apply(product: OnsetsAnalysisWindow.Product, config: Config = Config()): MultiResOnsets = {
    import config._

    @tailrec def loop(res: Vec[Entry], p: OnsetsAnalysisWindow.Product, lvl: Int): Vec[Entry] = p match {
      case (_cfg, Some(_o)) +: tail =>
        val cfg: OnsetsAnalysis.Config = _cfg   // sucky IDEA
        val o: Vec[Long] = _o                   // sucky IDEA
        val maxFrame = cfg.fftSize / cfg.fftOverlap * maxDrift
        val (lost, next) = ((res, Vec.empty[Entry]) /: o) {
          case ((rem, add), pos) =>
            val cand = rem.zipWithIndex.collect {
              case (entry, idx) if math.abs(entry.pos - pos) <= maxFrame => math.abs(entry.pos - pos) -> idx
            }
            if (cand.isEmpty) {
              (rem, add :+ Entry(pos, lvl, lvl + 1))
            } else {
              val idx     = cand.minBy(_._1)._2
              val up      = rem(idx)
              val rem1    = rem.patch(idx, Vec.empty, 1)
              (rem1, add :+ up.bump(pos))
            }
        }
        // if (lost.nonEmpty) println(s"Warning: lost ${lost.mkString(", ")} in level ${lvl + 1}")
        loop((lost ++ next).sortBy(_.pos), tail, lvl + 1)

      case _ :+ _ => sys.error(s"No onsets in level ${lvl + 1}")
      case _ => res
    }

    MultiResOnsets(product.size, loop(Vec.empty, product, 0))
  }

  case class Entry(pos: Long, from: Int, to: Int) {
    def levels: Int = to - from
    def bump(pos: Long): Entry = copy(to = to + 1)
  }
}
case class MultiResOnsets(levels: Int, onsets: Vec[MultiResOnsets.Entry])