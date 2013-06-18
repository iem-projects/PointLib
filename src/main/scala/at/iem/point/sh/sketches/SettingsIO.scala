package at.iem.point.sh.sketches

import java.io.{FileInputStream, FileOutputStream, OutputStreamWriter, File}
import at.iem.point.sh.sketches.genetic.{Formats, Settings}
import play.api.libs.json.Json

object SettingsIO {
  def write(settings: Settings, file: File) {
    val json  = Json.toJson(settings)(Formats.settings)
    val w     = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
    w.write(Json.prettyPrint(json))
    w.flush()
    w.close()
  }

  def read(file: File): Settings = {
    val fis   = new FileInputStream(file)
    val sz    = fis.available()
    val arr   = new Array[Byte](sz)
    fis.read(arr)
    fis.close()
    val str   = new String(arr, "UTF-8")
    val json  = Json.parse(str)
    val res   = Json.fromJson[Settings](json)(Formats.settings)
    res.getOrElse(sys.error("JSON decoding failed"))
  }
}