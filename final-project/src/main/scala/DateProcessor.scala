import scala.util.Try

//ข้อมูลใหม่ที่เพิ่ม day month year
case class AccidentWithDate(
  age: String,
  sex: String,
  homeLoc: Location,
  deadDate: String,
  incidentLoc: Location,
  day: Int,
  month: Int,
  year: Int
)

object DataProcessor {

  //รับ AccidentRecord แล้วคืน Option
  def splitDate(rec: AccidentRecord): Option[AccidentWithDate] =
    rec.deadDate.trim.split("-") match
      case Array(year, month, day) =>
        Try {
          AccidentWithDate(
            rec.age,
            rec.sex,
            rec.homeLoc,
            rec.deadDate,
            rec.incidentLoc,
            day.toInt,
            month.toInt,
            year.toInt
          )
        }.toOption

      case _ => None

}
