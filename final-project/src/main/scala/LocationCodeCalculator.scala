import Models.Location

object LocationCodeCalculator {
  // เปลี่ยนจาก mutable.Map เป็น Map (Immutable)
  def getLocationCode(
    location: Location, 
    provinceMap: Map[String, String], 
    districtMap: Map[String, String]
  ): String =
    districtMap.get(location.district) match
      case Some(code) => code
      case None =>
        provinceMap.get(location.province).getOrElse("N/A")
}