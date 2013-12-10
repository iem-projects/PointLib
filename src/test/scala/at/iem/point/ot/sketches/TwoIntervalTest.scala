    package at.iem.point.ot.sketches

    import de.sciss.poirot._
    import Implicits._
    import de.sciss.kollflitz.Ops._

    /** This test verifies the approach of forbidding the appearance of two successive intervals in a chord. */
    object TwoIntervalTest extends App {
      implicit val m = Model()

      type Chord          = Vec[IntVar]
      type ChordSolution  = Vec[Int]

      def mkChord(vc: Int = 3, lo: Int = 0, hi: Int = 4): Chord = {
        val c = Vec.fill(vc)(IntVar(lo, hi))
        c.pairMap((h, l) => l #< h)
        c
      }

      def forbidIntervals(c: Chord, iLo: Int = 1, iHi: Int = 2): Unit = {
        require(c.size >= 3)
        c.combinations(3).foreach { case Vec(hi, mid, lo) =>
          val mLo = (mid - lo ) % iLo
          val mHi = (hi  - mid) % iHi

          // the co-presence of iLo and iHi is forbidden.
          // this can be formalised as forbidding the
          // modulus of both steps to be both zero.
          // since the modulus must be >= 0, this can
          // be further expressed as saying the sum
          // of the modulus must no be zero.

          mLo + mHi #!= 0
        }
      }

      val c = mkChord()
      forbidIntervals(c)

      val solutionsB  = Vec.newBuilder[ChordSolution]

      def addSolution(): Unit = solutionsB += c.map(_.value())

      val select    = search(c, firstFail, indomainMin)
      val result    = satisfyAll(select, addSolution)
      val solutions = solutionsB.result()

      println(s"Found ${solutions.size} solutions:")              // should be 7 solutions
      solutions.foreach(c => println(c.reverse.mkString(", ")))   // all but (0, 1, 3) and (1, 2, 4)
    }
