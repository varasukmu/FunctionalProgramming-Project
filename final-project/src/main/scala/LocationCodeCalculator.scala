import scala.collection.mutable.Map

// Import Location from Models
import Models.Location

object LocationCodeCalculator {
  def getLocationCode(location: Location, provinceMap: Map[String, String], districtMap: Map[String, String]): String =
    // ลองหาจาก district mapping ก่อน (แม่นยำกว่า)
    districtMap.get(location.district) match
      case Some(code) => code
      case None =>
        // ถ้าไม่พบ ให้ใช้ province mapping
        provinceMap.get(location.province).getOrElse("N/A")
}