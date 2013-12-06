package at.iem.point.ms.sketches

object MathUtil {
  /** Calculates the mean and standard deviation of a given vector
    *
    * @param   vec         the vector to analyse
    * @param   frameOff    0 if the whole vector is to be considered,
    *                      otherwise column offset
    * @param   frameLen    the number of columns to analyse. if `-1`, uses `vec.size`
    *
    * @return              the tuple (mean, stddev)
    */
  def stat(vec: Vec[Double], frameOff: Int = 0, frameLen: Int = -1): (Double, Double) = {
    val frameLen1 = if (frameLen < 0) vec.size else frameLen
    val frameStop = frameOff + frameLen1
    var sum = 0.0
    var i = frameOff
    while (i < frameStop) {
      sum += vec(i)
      i += 1
    }

    val matSize = frameLen1
    val mean = sum / matSize
    sum = 0.0
    i = frameOff
    while (i < frameStop) {
      val d = vec(i) - mean
      sum += d * d
      i += 1
    }

    val stddev = math.sqrt(sum / matSize)
    (mean, stddev)
  }
}