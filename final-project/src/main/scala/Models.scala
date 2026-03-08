object Models {
  case class Location(district: String, province: String)
  case class AccidentRecord(
      age: String,
      sex: String,
      homeLoc: Location,
      deadDate: String,
      incidentLoc: Location
  )

  case class LocationWithCode(
      district: String,
      province: String,
      code: String
  )

  case class AccidentRecordWithCode(
      age: String,
      sex: String,
      homeLoc: LocationWithCode,
      deadDate: String,
      incidentLoc: LocationWithCode,
      distanceLevel: Int
  )
}
