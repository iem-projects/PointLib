package at.iem.point.er.sketches
package impl

import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.{FileDialog, Window}

import scala.swing.BorderPanel
import de.sciss.synth
import de.sciss.file._
import play.api.libs.json.{JsError, JsSuccess, Format, Json}
import java.io.{FileInputStream, FileOutputStream}
import de.sciss.play.json.{Formats, AutoFormat}

class OnsetsAnalysisWindowImpl(doc: Document) extends OnsetsAnalysisWindow with WindowImpl {
  import doc.{file => in}

  def handler = Main.windowHandler
  override def style   = Window.Palette

  // private val fileSpec  = AudioFile.readSpec(in)
  private val oCfg      = new OnsetsAnalysis.ConfigBuilder
  oCfg.input    = in

  import OnsetsAnalysisWindow.Product

  private val setView   = new OnsetsAnalysisSettingsView(doc, init = oCfg)
  private val listView  = SettingsListView[OnsetsAnalysis.ConfigAndProduct](setView) { case (cfg, _) =>
    import synth._
    f"t=${cfg.thresh}%1.2f, r=${cfg.fftSize}/${cfg.fftOverlap}, m=${cfg.median}, g=${cfg.minGap}, i=${cfg.inputGain.ampdb}%1.1f, n=${cfg.noiseFloor.ampdb}%1.1f, d=${cfg.decay}%1.1f"
  }

  def product: Product = listView.items
  def product_=(value: Product): Unit = {
    listView.items = value
    value.headOption.foreach(setView() = _)
  }

  private def defaultConfigFile = {
    val desktop = userHome / "Desktop"
    val dir = if (desktop.isDirectory) desktop else userHome
    dir / s"${in.base}_Onsets.json"
  }

  private lazy val jsonFormat: Format[Product] = {
    import Formats.{FileFormat, VecFormat, Tuple2Format}  // note: FileFormat _is_ used
    import OnsetsAnalysis.{Function, Config, ConfigAndProduct}
    implicit val fmtFunc: Format[Function] = AutoFormat[Function]
    implicit val fmtCfg : Format[Config  ] = AutoFormat[Config  ]
    Format.GenericFormat[Vec[ConfigAndProduct]]
  }

  def save(): Unit = {
    val p   = product
    if (p.isEmpty) return

    val dlg = FileDialog.save(init = Some(defaultConfigFile), title = "Export Onsets Settings")
    dlg.show(Some(this)).foreach { f =>
      implicit val fmt = jsonFormat
      val js  = Json.toJson[Product](product)
      val jss = Json.prettyPrint(js)
      // println(jss)
      val fos = new FileOutputStream(f)
      try {
        fos.write(jss.getBytes("UTF-8"))
      } finally {
        fos.close()
      }
    }
  }

  def load(): Unit = {
    val dlg = FileDialog.open(init = Some(defaultConfigFile), title = "Import Onsets Settings")
    dlg.filter = Some(_.ext.toLowerCase == "json")
    dlg.show(Some(this)).foreach { f =>
      implicit val fmt = jsonFormat
      val fis = new FileInputStream(f)
      val jss = try {
        val arr = new Array[Byte](fis.available())
        fis.read(arr)
        new String(arr, "UTF-8")
      } finally {
        fis.close()
      }
      val js  = Json.parse(jss)
      val jsr = Json.fromJson[Product](js)
      jsr match {
        case JsSuccess(prod, _) => product = prod
        case JsError(seq) => seq.foreach(println)
      }
    }
  }

  // ---- open window ----

  title = "Onsets Analysis Settings"
  // peer.getRootPane.putClientProperty("Window.style", "small")
  closeOperation = Window.CloseHide
  contents = new BorderPanel {
    add(setView .component, BorderPanel.Position.West)
    add(listView.component, BorderPanel.Position.East)
  }
  pack()
  resizable = false
  // this.placeRightOf(top)
  front()
}