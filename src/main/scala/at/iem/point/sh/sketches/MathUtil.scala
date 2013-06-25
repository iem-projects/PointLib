package at.iem.point.sh.sketches

import collection.immutable.{IndexedSeq => Vec}

object MathUtil {
  /** Calculates the mean and standard deviation of a given vector
    *
    * @param   vec         the vector to analyse
    * @param   frameOff    0 if the whole vector is to be considered,
    *                      otherwise column offset
    * @param   frameLen    the number of columns to analyse
    *
    * @return              the tuple (mean, stddev)
    */
  def stat(vec: Vec[Double], frameOff: Int, frameLen: Int): (Double, Double) = {
    val frameStop = frameOff + frameLen
    var sum = 0.0
    var i = frameOff
    while (i < frameStop) {
      sum += vec(i)
      i += 1
    }

    val matSize = frameLen
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

  //  /** Performs cross correlation between two horizontal halves of a matrix 'a'.
  //    *
  //    * For efficiency reasons, a may be updated in a rotational manner, thus frameOff + halfWinSize
  //    * may exceed the number of frames in a (the algorithm automatically takes the modulus).
  //    *
  //    * @param   chanOff     the channel or row offset in the matrix
  //    * @param   numChannels the number of channels or rows to process in the matrix
  //    * @param   halfWinSize half of the number of frames or columns in the matrix. The algorithm
  //    *                      performs a cross correlation between the first half frame beginning at offset
  //    *                      `frameOff` and the second half beginning at `frameOff + halfWinSize` (possibly
  //    *                      wrapped around the array boundaries)
  //    * @param   a           the matrix to analyse
  //    *
  //    * @return  the cross correlation coefficient (sum of cell multiplies, divided by product of
  //    *          variance and matrix size)
  //    */
  //  def correlateHalf(numChannels: Int, halfWinSize: Int, a: Array[Array[Float]], frameOff: Int, chanOff: Int): Float = {
  //    val numFrames = halfWinSize << 1
  //    val (mean, stdDev) = stat(a, 0, numFrames, chanOff, numChannels)
  //    val add = -mean
  //    val matSize = numChannels * halfWinSize
  //
  //    var sum = 0.0
  //    var ch = 0
  //    while (ch < numChannels) {
  //      val ca = a(ch + chanOff)
  //      var i = frameOff
  //      val j = frameOff + halfWinSize
  //      while (i < j) {
  //        sum += (ca(i % numFrames) + add) * (ca((i + halfWinSize) % numFrames) + add)
  //        i += 1
  //      }
  //      ch += 1
  //    }
  //    (sum / (stdDev * stdDev * matSize)).toFloat // ensures correlate( a, a ) == 1.0
  //  }

  //  /** Calculates the mean of a vector
  //    *
  //    * @param   b  the vector
  //    * @param   off   the offset into the vector
  //    * @param   len   the number of samples to take into account
  //    *
  //    * @return  the average of the `len` samples starting at offset `off` in vector `b`.
  //    */
  //  def avg(b: Array[Float], off: Int, len: Int): Float = {
  //    var sum = 0.0
  //    var i = off
  //    val stop = off + len
  //    while (i < stop) {
  //      sum += b(i)
  //      i += 1
  //    }
  //    (sum / len).toFloat
  //  }

  //  /** Normalizes a matrix with respect to a normalization vector. For each row in the matrix, given
  //    * the rows minimum and maximum value through the normalization vector, every cell is offset
  //    * by `-minimum` and then divided by `maximum - minimum`.
  //    *
  //    * @param   normBuf  provides normalization vector. The outer array must be of size `b.length`.
  //    *                   each element of that outer array holds an array of size 2, specifying
  //    *                   minimum and maximum value for the index. The `normBuf` argument may be
  //    *                   `null`, in which case this method simply returns without any adjustments.
  //    * @param   b        the matrix which is normalized in-place (the values are scaled and overwritten).
  //    * @param   bOff     a frame of column offset in the matrix `b`.
  //    * @param   bLen     the number of frames or columns to process in the matrix `b`.
  //    */
  //  def normalize(normBuf: Array[Array[Float]], b: Array[Array[Float]], bOff: Int, bLen: Int) {
  //    if (normBuf == null) return
  //    var ch = 0
  //    val numCh = b.length
  //    while (ch < numCh) {
  //      val cb = b(ch)
  //      val cn = normBuf(ch)
  //      val min = cn(0)
  //      val max = cn(1)
  //      val d = max - min
  //      var i = bOff
  //      val iStop = bOff + bLen
  //      while (i < iStop) {
  //        val f = cb(i)
  //        // XXX should values be clipped to [0...1] or not?
  //        cb(i) = (f - min) / d
  //        i += 1
  //      }
  //      ch += 1
  //    }
  //  }

  /** Performs cross correlation between two vectors a and b. Because either `a` or `b` may be static,
    * the method expects the means and standard deviations of vectors to be passed in.
    *
    * While `a` is considered a vector with at least `numFrames` columns, offset can be given for the `b` vector.
    *
    * For efficiency reasons, `b` may be updated in a rotational manner, thus `bFrame + frameLen`
    * may exceed the number of frames in `b`. The algorithm automatically takes the modulus
    * `bFrame + frameLen % b.numFrames` as offset when doing the calculations.
    *
    * @param   a           the first vector
    * @param   b           the second vector
    * @param   aMean       the mean of the samples in `a`
    * @param   bMean       the mean of the samples in `b`
    * @param   aStdDev     the standard deviation of the samples in `a`
    * @param   bStdDev     the standard deviation of the samples in `b`
    * @param   numFrames   the number of columns in `a`, also the number of columns considered in the correlation
    * @param   bFrameOff   frame or column offset in `b`
    *
    * @return  the cross correlation coefficient
    */
  def correlate(a: Vec[Double], aMean: Double, aStdDev: Double, numFrames: Int,
                b: Vec[Double], bMean: Double, bStdDev: Double, bFrameOff: Int): Double = {
    val aAdd = -aMean
    val bAdd = -bMean
    val aMatSize = numFrames

    var sum = 0.0

    var i = 0
    while (i < numFrames) {
      sum += (a(i) + aAdd) * (b((i + bFrameOff) % b.length) + bAdd)
      i += 1
    }

    sum / (aStdDev * bStdDev * aMatSize) // ensures correlate( a, a ) == 1.0
  }
}