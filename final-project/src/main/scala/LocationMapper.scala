import scala.io.Source
import scala.util.Using

object LocationMapper {

  /** Pure helper that parses the CSV content already split into lines. */
  def parseMappingLines(
      lines: List[String],
      minCols: Int,
      keyIdx: Int,
      valIdx: Int
  ): Map[String, String] =
    lines
      .drop(1)
      .flatMap { line =>
        val cols = line.split(",").map(_.trim.replaceAll("\"", ""))
        if (cols.length >= minCols) Some(cols(keyIdx) -> cols(valIdx)) else None
      }
      .toMap

  private def readFile(fileName: String): Either[String, List[String]] =
    Using(Source.fromFile(fileName, "UTF-8")) { src =>
      src.getLines().toList
    }.toEither.left.map(e => s"File error: ${e.getMessage}")

  def loadProvinceMapping(
      fileName: String
  ): Either[String, Map[String, String]] =
    readFile(fileName).map(
      parseMappingLines(_, 4, 2, 1)
    ) // Province Name -> Code

  def loadDistrictMapping(
      fileName: String
  ): Either[String, Map[String, String]] =
    readFile(fileName).map(
      parseMappingLines(_, 6, 4, 3)
    ) // District Name -> Code
}
