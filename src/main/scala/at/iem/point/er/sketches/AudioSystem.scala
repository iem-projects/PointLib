package at.iem.point.er.sketches

import impl.{AudioSystemImpl => Impl}
import de.sciss.synth
import synth.Model
import de.sciss.osc.TCP

object AudioSystem {
  def instance: AudioSystem = Impl.instance

  def start(config: synth.Server.Config = defaultConfig): AudioSystem = instance.start(config)

  sealed trait Update
  final case class Booting(connection: synth.ServerConnection) extends Update
  final case class Started(server: synth.Server) extends Update
  case object Stopped extends Update

  type Listener = Model.Listener[Update]

  lazy val defaultConfig = {
    val cfg       = synth.Server.Config()
    cfg.transport = TCP
    cfg.build
  }
}
trait AudioSystem extends Model[AudioSystem.Update] {
  def server: Option[synth.ServerLike]
  def start(config: synth.Server.Config = AudioSystem.defaultConfig): this.type
  def stop(): this.type

  def isBooting: Boolean
  def isRunning: Boolean
}