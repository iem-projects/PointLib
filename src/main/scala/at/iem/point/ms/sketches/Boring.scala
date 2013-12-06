package at.iem.point.ms.sketches

import scala.swing.Swing
import GUI._
import Swing._
import at.iem.point.illism._
import at.iem.point.illism.rhythm.{Ladma, numericCompat}
import de.sciss.file._

object Boring extends App {
  Swing.onEDT {
    val which = args.headOption.getOrElse("?")

    which match {
      case "--kreuz"  => kreuz()
      case "--ladma"  => ladmaVerlauf()
      case "--harm"   => harmonicVerlauf()

      case _ =>
        println(
          """Options:
            |
            |--kreuz  Interval cross table
            |--ladma  Ladma progressions
            |--harm   Harmonic ambitus progressions
            |
            |""".stripMargin)
        sys.exit(1)
    }
  }

  def kreuz(): Unit = {
    val ins   = newBoring ++ newPromising

    val panes = ins.map { study =>
      Seq(true, false).map { all =>
        // val ins = Seq(Study.Boring(26), Study.Raw(5))
        Kreuztabelle.analyzeAndPlot(study, allIntervals = all)
      }
    }
    panes.foreach { p =>
      val panel = p.asGrid(2, 1)
      frame("Kreuztabelle", panel, (600, 1200))
    }
  }

  def ladmaVerlauf(): Unit = {
    val measure = "mobility"

    // val sts = Vector(Study.Boring(26), Study.Raw(5))
    // val sts = Vector(Study.Boring(29), Study.Raw(4), Study.Raw(5), Study.Raw(6) /*, Study.Raw(7) */)

    // val sts     = Vector(Study.Boring(26)) // , Study.Boring(29)) // , Study.Boring(31))
    // val sts     = Vector(Study.Raw(4), Study.Raw(5), Study.Raw(6)) // , Study.Raw(7))
    val sts     = Vector(Study.Boring(29), Study.Raw(4), Study.Raw(5), Study.Raw(6))
    val winSecs = 16.0    // window size in seconds
    val winOver = 8       // window step factor (8 means 1/8 of window size)

    val ms  = sts.map { st =>
      println(s"\nFile: ${st.file.name}")
      val notes     = load(st).notes
      val tup = {
        // the problem with findHarmonicFields is that they overlap
        // val segm      = ChordUtil.findHarmonicFields(notes, minPoly = 1)
        // segm.map(c => c.avgOffset -> c.avgDuration)
        val tFlt  = NoteUtil.clean(notes, minDuration = 0.1)
        val stabs = NoteUtil.stabbings(tFlt)
        val _durs = stabs.pairDiff
        stabs zip _durs
      }
      val (segm, durRaw) = tup.unzip
      // val quant     = Tempo.guess(durRaw, maxDenom = if (st.isInstanceOf[Study.Boring]) 128 else 128)
      val quant = Tempo.quantize(durRaw, qpm = 160, maxDenom = 64)
      println(s"Tempo: ${quant.tempoBase} = ${quant.tempoNom}")

      val winSize = winSecs / quant.wholeDuration  // wholes per 10 seconds
      val winStep = winSize / winOver
      println(f"Sliding window size $winSize, step $winStep (1 = ${quant.wholeDuration}%1.3fs)")

      assert(segm.size == quant.durations.size)
      val segmQuant = segm zip quant.durations
      val slices0   = NoteUtil2.slidingWindow(segmQuant, size = winSize, step = winStep)(_._1)
      val slices    = slices0.dropRight(winOver*2/3) // last slice is smaller and produces spikes

      val cells     = slices.zipWithIndex.flatMap { case (xs, i) =>
        val dursQuant = xs.map(_._2)
        val dur = dursQuant.sum
        if (dur == 0) None else Some(
          rhythm.Cell(i, dursQuant.map(rhythm.Note), dur)
        )
      }
      val mf  = measure match {
        case "entropy"  => Ladma.entropy  _
        case "mobility" => Ladma.mobility _
        case "tension"  => Ladma.tension  _
      }
      val m = cells.map(mf)
      m
    }

    import Plotting._

    ms.plot(ylabel = measure.capitalize, title = s"${measure.capitalize} Comparison", legends = sts.map(_.file.name))
  }

  def harmonicVerlauf(): Unit = {
    // val sts     = newBoring // Vector(Study.Boring(26), Study.Boring(29), Study.Raw(5), Study.Raw(10))
    val sts = newPromising
    sts.grouped(3).foreach(harmonicCore)
  }

  object Measure {
    sealed trait Chord extends Measure { def isChord = true }
    case object ChordVar  extends Chord
    case object ChordMean extends Chord

    sealed trait Horiz extends Measure { def isChord = false }
    case object HorizMean extends Horiz
    case object HorizVar  extends Horiz
    case object HorizAmbi extends Horiz
  }
  sealed trait Measure {
    def isChord: Boolean
    def isHoriz = !isChord
    def name    = toString
  }

  def process(study: Study, measure: Measure, ivalClass: Boolean = false,
              winSecs: Double = 16.0, winOver: Int = 8): Vec[Double] = {
    val midi      = load(study)
    val winSize   = winSecs
    val winStep   = winSize / winOver

    measure match {
      case mchord: Measure.Chord =>
        val notes     = midi.notes
        val segm      = ChordUtil.findHarmonicFields(notes, minPoly = 1).sortBy(_.minOffset)

        val slices0   = NoteUtil2.slidingWindow(segm, size = winSize, step = winStep)(_.minOffset)
        val slices    = slices0.dropRight(winOver*2/3) // last slice is smaller and produces spikes
        val m         = slices.map { slice =>
            val chordM = slice.flatMap { chord =>
              chord.allIntervals.map { ival =>
                val steps = if (ivalClass) ival.`class`.steps else ival.semitones
                steps.toDouble
              }
            }
            mchord match {
              case Measure.ChordMean => chordM.meanVariance._1 / slice.size
              case Measure.ChordVar  => chordM.meanVariance._2 / slice.size
            }
          }
        m

      case mhoriz: Measure.Horiz =>
        val segm0 = (0 until 4).flatMap { ch =>
          val notes: Vec[OffsetNote] = midi.notes(channel = ch)
          notes.map(_.offset).drop(1) zip notes.map(_.pitch.midi).pairDiff.map(_.abs)  // (offset, directed interval)
        }
        val segm = segm0.sortBy(_._1)
        val slices0   = NoteUtil2.slidingWindow(segm, size = winSize, step = winStep)(_._1)
        val slices    = slices0.dropRight(winOver*2/3) // last slice is smaller and produces spikes

        slices.map { sq =>
          val ivals = sq.map(_._2.toDouble)
          val (mean, vr) = ivals.meanVariance
          mhoriz match {
            case Measure.HorizMean  => mean
            case Measure.HorizVar   => vr
            case Measure.HorizAmbi  => ivals.max - ivals.min
          }
        }
    }
  }

  private def harmonicCore(sts: Vec[Study]): Unit = {
    val measure   = Measure.ChordVar: Measure
    val ivalClass = false
    // val sts     = Vector(Study.Raw(4), Study.Raw(5), Study.Raw(6), Study.Raw(7))
    // val sts     = Vector(Study.Raw(8), Study.Raw(9), Study.Raw(10), Study.Raw(11))
    // val sts     = Vector(Study.Boring(26), Study.Boring(29), Study.Boring(31))
    val winSecs = 16.0
    val winOver = 8

    val ms  = sts.map { study =>
      println(s"\nFile: ${study.file.name}")
      process(study = study, measure = measure, ivalClass = ivalClass, winSecs = winSecs, winOver = winOver)
    }

    import Plotting._

    val titleDetail = if (measure.isChord)
      "inner-harmonic intervals"
    else
      "successive pitch steps"

    ms.plot(ylabel = s"${measure.name}", title = s"Diversity in $titleDetail",
      legends = sts.map(_.file.name))
  }
}