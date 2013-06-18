package at.iem.point.sh.sketches

import java.io.{FileInputStream, InputStreamReader, FileOutputStream, OutputStreamWriter, FileReader, FileWriter, File}
import at.iem.point.sh.sketches.genetic.{Formats, Evaluation}
import play.api.libs.json.Json

object EvalIO {
  private case class Format(evaluation: Evaluation)
  import Formats.{evaluation => ev}
  private implicit val fmt = Json.format[Format]

  def write(evaluation: Evaluation, file: File) {
    val json  = Json.toJson(Format(evaluation))
    val w     = new OutputStreamWriter(new FileOutputStream(file), "UTF-8")
    w.write(Json.prettyPrint(json))
    w.flush()
    w.close()
  }

  def read(file: File): Evaluation = {
    val fis   = new FileInputStream(file)
    val sz    = fis.available()
    val arr   = new Array[Byte](sz)
    fis.read(arr)
    fis.close()
    val str   = new String(arr, "UTF-8")
    val json  = Json.parse(str)
    val res   = Json.fromJson[Format](json)
    res.getOrElse(sys.error("JSON decoding failed")).evaluation
  }
}