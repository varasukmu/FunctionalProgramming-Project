import scala.io.Source
import scala.util.{Using, Try}
import scala.collection.mutable.Map

// กำหนดโครงสร้างข้อมูล
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
  birthDate: String,
  deadDate: String,
  incidentLoc: LocationWithCode,
  distanceLevel: Int
)

@main def runAnalysis(): Unit =
  val fileName = "Dataset.csv" // กำหนดชื่อไฟล์ไ
  val outputFile = "newDataset.csv"

  // โหลด mapping สำหรับรหัสไปรษณีย์
  val provinceMap = loadProvinceMapping("provinces.csv")
  val districtMap = loadDistrictMapping("districts.csv")

  extractAccidentData(fileName) match
    case Right(data) =>
      println(s"Success! Found ${data.size} clean records.")

      // แปลงข้อมูลและเพิ่มรหัสไปรษณีย์
      val dataWithCodes = data.map { rec =>
        val homeCode = getLocationCode(rec.homeLoc, provinceMap, districtMap)
        val incidentCode = getLocationCode(rec.incidentLoc, provinceMap, districtMap)
        val distanceLevel = calculateDistanceLevel(rec.homeLoc, rec.incidentLoc)
        val birthDate = calculateBirthDate(rec.age, rec.deadDate)

        AccidentRecordWithCode(
          rec.age,
          rec.sex,
          LocationWithCode(rec.homeLoc.district, rec.homeLoc.province, homeCode),
          birthDate,
          rec.deadDate,
          LocationWithCode(rec.incidentLoc.district, rec.incidentLoc.province, incidentCode),
          distanceLevel
        )
      }

      // Write to CSV file
      val writer = java.io.FileWriter(outputFile)
      try {
        // Write CSV header
        writer.write("Age,Sex,Home_District,Home_Province,Home_Code,Birth_Date,Dead_Date,Incident_District,Incident_Province,Incident_Code,Distance_Level\n")
        
        // Write each record as CSV row
        dataWithCodes.foreach { rec =>
          val row = s"${rec.age},${rec.sex},${rec.homeLoc.district},${rec.homeLoc.province},${rec.homeLoc.code},${rec.birthDate},${rec.deadDate},${rec.incidentLoc.district},${rec.incidentLoc.province},${rec.incidentLoc.code},${rec.distanceLevel}\n"
          writer.write(row)
        }

        println(s"Cleaned data with postal codes exported to: $outputFile")
        println(s"Total records: ${dataWithCodes.size}")

      } finally {
        writer.close()
      }

    case Left(error) =>
      System.err.println(s"Error: $error")

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

def extractAccidentData(fileName: String): Either[String, List[AccidentRecord]] =
  Using(Source.fromFile(fileName, "UTF-8")) { source =>
    val allLines = source.getLines().toList

    allLines match
      case Nil => Nil
      case headerLine :: dataLines =>
        val header = headerLine.split(",").map(_.trim).toList
        val idx = (name: String) => header.indexOf(name)

        // ดึงตำแหน่ง column
        val colsMap = Map(
          "age" -> idx("Age"),
          "sex" -> idx("Sex"),
          "dist" -> idx("District"),
          "prov" -> idx("Province"),
          "date" -> idx("Dead Date"),
          "accDist" -> idx("acc_district_name"),
          "accProv" -> idx("จ.ที่เสียชีวิต")
        )

        dataLines.flatMap { line =>
          val c = line.split(",", -1).map(_.trim)

          // helper เพื่อดึงค่าและเช็คว่างแบบ Option
          def get(key: String): Option[String] =
            colsMap.get(key).filter(i => i >= 0 && i < c.length && c(i).nonEmpty).map(c(_))

          // Functional Composition: ถ้าได้ครบทุกค่าถึงจะสร้าง AccidentRecord
          for {
            age  <- get("age")
            sex  <- get("sex")
            d    <- get("dist")
            p    <- get("prov")
            date <- get("date")
            ad   <- get("accDist")
            ap   <- get("accProv")
          } yield AccidentRecord(age, sex, Location(d, p), date, Location(ad, ap))
        }
  }.toEither.left.map(e => s"File error: ${e.getMessage}")

def getLocationCode(location: Location, provinceMap: Map[String, String], districtMap: Map[String, String]): String =
  // ลองหาจาก district mapping ก่อน (แม่นยำกว่า)
  districtMap.get(location.district) match
    case Some(code) => code
    case None =>
      // ถ้าไม่พบ ให้ใช้ province mapping
      provinceMap.get(location.province).getOrElse("N/A")

def calculateBirthDate(ageStr: String, deadDateStr: String): String =
  try {
    val age = ageStr.toInt
    // Dead_Date format: YYYY-MM-DD
    val year = deadDateStr.split("-")(0).toInt
    val birthYear = year - age
    s"$birthYear-01-01" // ใช้ 1 มกราคมเป็นวันเกิดโดยประมาณ
  } catch {
    case _: Exception => "N/A"
  }

def calculateDistanceLevel(homeLoc: Location, incidentLoc: Location): Int =
  val sameProvince = homeLoc.province == incidentLoc.province
  val sameDistrict = homeLoc.district == incidentLoc.district

  if (sameProvince && sameDistrict) 0
  else if (sameProvince && !sameDistrict) 1
  else if (!sameProvince && !sameDistrict) 2
  else -1