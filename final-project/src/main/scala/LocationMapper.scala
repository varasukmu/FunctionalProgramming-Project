import scala.io.Source
import scala.collection.mutable.Map
import scala.util.Using

object LocationMapper {
  def loadProvinceMapping(fileName: String): Map[String, String] =
    val mapping = Map[String, String]()
    Using(Source.fromFile(fileName, "UTF-8")) { source =>
      val lines = source.getLines().drop(1) // ข้าม header
      lines.foreach { line =>
        val cols = line.split(",").map(_.trim)
        if (cols.length >= 4) {
          val provinceName = cols(2).replaceAll("\"", "") // PROVINCE_THAI
          val code = cols(1).replaceAll("\"", "") // CODE
          mapping(provinceName) = code
        }
      }
    }
    mapping

  def loadDistrictMapping(fileName: String): Map[String, String] =
    val mapping = Map[String, String]()
    Using(Source.fromFile(fileName, "UTF-8")) { source =>
      val lines = source.getLines().drop(1) // ข้าม header
      lines.foreach { line =>
        val cols = line.split(",").map(_.trim)
        if (cols.length >= 6) {
          val districtName = cols(4).replaceAll("\"", "") // DISTRICT_THAI
          val code = cols(3).replaceAll("\"", "") // DISTRICT_CODE
          mapping(districtName) = code
        }
      }
    }
    mapping
}