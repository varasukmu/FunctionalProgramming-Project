package Models

case class Location(
    district: String, 
    province: String)

case class LocationWithCode(
    district: String,
    province: String, code: String)

case class AccidentRecord(
    age: String,
    sex: String,
    homeLoc: Location,
    deadDate: String,
    incidentLoc: Location
)

case class AccidentRecordWithCode(
    age: String,
    sex: String,
    homeLoc: LocationWithCode,
    deadDate: String,
    incidentLoc: LocationWithCode,
    distanceLevel: Int
)

case class AccidentWithDate(
    age: String,
    sex: String,
    homeLoc: LocationWithCode,
    incidentLoc: LocationWithCode,
    distanceLevel: Int,
    day: Int,
    month: Int,
    year: Int
)