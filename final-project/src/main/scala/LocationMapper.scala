import scala.io.Source
import scala.util.Using

object LocationMapper {
  private def loadMapping(fileName: String, minCols: Int, keyIdx: Int, valIdx: Int): Map[String, String] =
    Using(Source.fromFile(fileName, "UTF-8")) { source =>
      source.getLines()
        .drop(1)
        .flatMap { line =>
          val cols = line.split(",").map(_.trim.replaceAll("\"", ""))
          if (cols.length >= minCols) Some(cols(keyIdx) -> cols(valIdx)) else None
        }.toMap
    }.getOrElse(Map.empty)

  def loadProvinceMapping(fileName: String): Map[String, String] =
    loadMapping(fileName, 4, 2, 1) // Province Name -> Code

  def loadDistrictMapping(fileName: String): Map[String, String] =
    loadMapping(fileName, 6, 4, 3) // District Name -> Code
}