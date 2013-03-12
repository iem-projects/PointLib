package at.iem.point.eh.sketches.midi

import javax.sound.{midi => j}
import j.ShortMessage._
import annotation.switch
import collection.immutable.{IndexedSeq => IIdxSeq}
import util.control.NonFatal

object Message {
  def fromJavaOption(m: j.MidiMessage): Option[Message] = {
    try { Some(fromJava(m)) } catch { case NonFatal(_) => None }
  }

  def fromJava(m: j.MidiMessage): Message = {
    m match {
      case sm: j.ShortMessage =>
        val channel = sm.getChannel
        (sm.getCommand: @switch) match {
          case NOTE_ON if sm.getData2 > 0 =>
            NoteOn (channel, sm.getData1, sm.getData2)
          case NOTE_OFF =>
            NoteOff (channel, sm.getData1, sm.getData2)
          case NOTE_ON /* if sm.getData2 == 0 */ =>   // retarded MIDI spec: NoteOn with velocity 0 replaces NoteOff
            NoteOff (channel, sm.getData1, sm.getData2)
          case _ => unknownMessage(m)
        }
      case mm: j.MetaMessage =>
        import MetaMessage._
        // cf. http://www.omega-art.com/midi/mfiles.html#meta
        (mm.getType: @switch) match {
          case KeySignature.tpe =>
            val arr = mm.getData
//println("LENGTH = " + arr.length + " FIRST = " + arr(0))
//            if (arr.length != 3 || arr(0) != KeySignature.subType) unknownMessage(m)
//            KeySignature(arr(1), KeySignature.Mode(arr(2)))
            // why is the subtype swallowed?
            if (arr.length != 2) unknownMessage(m)
            KeySignature(arr(0), KeySignature.Mode(arr(1)))

          case EndOfTrack.tpe =>
            val arr = mm.getData
//            if (arr.length != 1 || arr(0) != EndOfTrack.subType) unknownMessage(m)
            if (arr.length != 0) unknownMessage(m)
            EndOfTrack

          case TimeSignature.tpe =>
            val arr             = mm.getData
//println("LENGTH = " + arr.length + " FIRST = " + arr(0))
//            if (arr.length != 5 || arr(0) != TimeSignature.subType) unknownMessage(m)
//            val num             = arr(1)
//            val denom           = 1 << arr(2)
//            val clocksPerMetro  = arr(3)
//            val num32perQ       = arr(4)
            if (arr.length != 4) unknownMessage(m)
            val num             = arr(0)
            val denom           = 1 << arr(1)
            val clocksPerMetro  = arr(2)
            val num32perQ       = arr(3)
            TimeSignature(num, denom, clocksPerMetro, num32perQ)

          case SetTempo.tpe =>
            val arr = mm.getData
            if (arr.length != 3) unknownMessage(m)
            val microsPerQ  = ((arr(0) & 0xFF) << 16) | ((arr(1) & 0xFF) << 8) | (arr(2) & 0xFF)
            SetTempo(microsPerQ)

//          case TrackName.tpe =>
//            val arr = mm.getData
//            require(arr(0) == arr.length - 1)


          case _ => unknownMessage(m)
        }

      case xm: j.SysexMessage =>
        SysExMessage(xm.getData.toIndexedSeq)
    }
  }

  @inline private def unknownMessage(m: j.MidiMessage): Nothing =
    throw new IllegalArgumentException("Unsupported MIDI message " +
      m.getMessage.map(b => (b & 0xFF).toHexString).mkString("[", ",", "]"))
}
sealed trait Message {
  def toJava: j.MidiMessage
}
final case class NoteOn(channel: Int, pitch: Int, velocity: Int) extends Message {
  override def toString = s"${productPrefix}(channel = ${channel}, pitch = ${pitch}, velocity = ${velocity})"

  def toJava: j.MidiMessage = {
    val res = new j.ShortMessage
    res.setMessage(NOTE_ON, channel, pitch, velocity)
    res
  }
}
final case class NoteOff(channel: Int, pitch: Int, velocity: Int) extends Message {
  override def toString = s"${productPrefix}(channel = ${channel}, pitch = ${pitch}, velocity = ${velocity})"

  def toJava: j.MidiMessage = {
    val res = new j.ShortMessage
    res.setMessage(NOTE_OFF, channel, pitch, velocity)
    res
  }
}
final case class SysExMessage(data: IIdxSeq[Byte]) extends Message {
  def toJava: j.MidiMessage = {
    val res = new j.SysexMessage
    val arr = data.toArray
    res.setMessage(arr, arr.length)
    res
  }
}
object MetaMessage {
  object KeySignature {
    final val tpe     = 0x59
    final val subType = 0x02

    object Mode {
      def apply(id: Int): Mode = (id: @switch) match {
        case Minor.id => Minor
        case Major.id => Major
        case _        => throw new IllegalArgumentException(s"Unknown key signature mode ${id}")
      }
    }
    sealed trait Mode { def id: Int }
    case object Minor extends Mode { final val id = 0 }
    case object Major extends Mode { final val id = 1 }
  }
  final case class KeySignature(shift: Int, mode: KeySignature.Mode) extends MetaMessage {
    override def toString = s"${productPrefix}(shift = ${shift}, ${mode})"

    def toJava: j.MidiMessage = {
      val res = new j.MetaMessage
      val arr = new Array[Byte](4)
      arr(0)  = KeySignature.subType.toByte
      arr(1)  = shift.toByte
      arr(2)  = mode.id.toByte
      res.setMessage(KeySignature.tpe, arr, arr.length)
      res
    }
  }

  case object EndOfTrack extends MetaMessage {
    final val tpe     = 0x2F
    final val subType = 0x00
    def toJava: j.MidiMessage = {
      val res = new j.MetaMessage
      val arr = new Array[Byte](1)
      arr(0)  = subType.toByte
      res.setMessage(tpe, arr, arr.length)
      res
    }
  }

  object TimeSignature {
    final val tpe     = 0x58
    final val subType = 0x04

    private def isPowerOfTwo(value: Int) = (value & (value-1)) == 0

  }
  final case class TimeSignature(num: Int, denom: Int, clocksPerMetro: Int, num32perQ: Int = 32) extends MetaMessage {
    if (!TimeSignature.isPowerOfTwo(denom))
      throw new IllegalArgumentException(s"Denominator (${denom}) must be a power of two")
//    if (num < 0 || num > 255 || denom < 0)
//      throw new IllegalArgumentException(s"Values out of range (0 <= ${num} < 256, 0 <= ${denom})")

    override def toString = s"${productPrefix}(${num}/${denom}, clocksPerMetro = ${clocksPerMetro}, num32perQ = ${num32perQ})"

    def toJava: j.MidiMessage = {
      val res = new j.MetaMessage
      val arr = new Array[Byte](5)
      arr(0)  = TimeSignature.subType.toByte
      arr(1)  = num.toByte
      val denomP = {
        var i = 0
        var j = denom
        while (j > 1) {
          j >>= 1
          i  += 1
        }
        i
      }
      arr(2)  = denomP.toByte
      arr(3)  = clocksPerMetro.toByte
      arr(4)  = num32perQ.toByte
      res.setMessage(TimeSignature.tpe, arr, arr.length)
      res
    }
  }

  object SetTempo {
    final val tpe     = 0x51
    final val subType = 0x03

    def bpm(value: Double): SetTempo = {
      val microsPerQ = (60.0e6 / value + 0.5).toInt
      apply(microsPerQ)
    }
  }
  final case class SetTempo(microsPerQ: Int) extends MetaMessage {
    def bpm: Double = 60.0e6 / microsPerQ

    override def toString = s"${productPrefix}(µs per 1/4 = ${microsPerQ}, bpm = ${bpm.toInt})"

    def toJava: j.MidiMessage = {
      val res = new j.MetaMessage
      val arr = new Array[Byte](4)
      arr(0)  = SetTempo.subType.toByte
      arr(1)  = (microsPerQ >> 16).toByte
      arr(2)  = (microsPerQ >> 8).toByte
      arr(3)  =  microsPerQ.toByte
      res.setMessage(SetTempo.tpe, arr, arr.length)
      res
    }
  }

//  object TrackName {
//    final val tpe = 0x03
//  }
//  final case class TrackName(name: String) extends MetaMessage {
//
//  }
}
sealed trait MetaMessage extends Message