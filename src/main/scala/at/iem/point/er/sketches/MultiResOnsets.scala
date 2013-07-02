package at.iem.point.er.sketches

import scala.annotation.tailrec

object MultiResOnsets {
  case class Config(maxDrift: Int = 4)

  def apply(product: OnsetsAnalysisWindow.Product, config: Config = Config()): MultiResOnsets = {
    import config._

    @tailrec def loop(res: Vec[(Long, Int)], p: OnsetsAnalysisWindow.Product, lvl: Int): Vec[(Long, Int)] = p match {
      case (_cfg, Some(_o)) +: tail =>
        val cfg: OnsetsAnalysis.Config = _cfg   // sucky IDEA
        val o: Vec[Long] = _o                   // sucky IDEA
        val maxFrame = cfg.fftSize / cfg.fftOverlap * maxDrift
        val (lost, next) = ((res, Vec.empty[(Long, Int)]) /: o) {
          case ((rem, add), pos) =>
            val cand = rem.zipWithIndex.collect {
              case ((pos1, _), idx) if math.abs(pos1 - pos) <= maxFrame => math.abs(pos1 - pos) -> idx
            }
            if (cand.isEmpty) {
              (rem, add :+ (pos, 1))
            } else {
              val idx     = cand.minBy(_._1)._2
              val (_, up) = rem(idx)
              val rem1    = rem.patch(idx, Vec.empty, 1)
              (rem1, add :+ (pos, up + 1))
            }
        }
        if (lost.nonEmpty) println(s"Warning: lost ${lost.mkString(", ")} in level ${lvl + 1}")
        loop(next, tail, lvl + 1)

      case _ :+ _ => sys.error(s"No onsets in level ${lvl + 1}")
      case _ => res
    }

    MultiResOnsets(product.size, loop(Vec.empty, product, 0))
  }
}
case class MultiResOnsets(levels: Int, onsets: Vec[(Long, Int)])