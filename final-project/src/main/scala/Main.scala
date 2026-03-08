import scala.io.Source
import scala.util.{Using, Try}
import scala.collection.mutable.Map

// Import functions from separate files
import DataExtractor.{extractAccidentData}
import LocationMapper.{loadProvinceMapping, loadDistrictMapping}
import LocationCodeCalculator.{getLocationCode}
import Models.{Location, AccidentRecord, LocationWithCode, AccidentRecordWithCode}

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

        Models.AccidentRecordWithCode(
          rec.age,
          rec.sex,
          Models.LocationWithCode(rec.homeLoc.district, rec.homeLoc.province, homeCode),
          rec.deadDate,
          LocationWithCode(rec.incidentLoc.district, rec.incidentLoc.province, incidentCode),
          distanceLevel
        )
      }

      // Write to CSV file
      val writer = java.io.FileWriter(outputFile)
      try {
        // Write CSV header
        writer.write("Age,Sex,Home_District,Home_Province,Home_Code,Dead_Date,Incident_District,Incident_Province,Incident_Code,Distance_Level\n")
        
        // Write each record as CSV row
        dataWithCodes.foreach { (rec: Models.AccidentRecordWithCode) =>
          val row = s"${rec.age},${rec.sex},${rec.homeLoc.district},${rec.homeLoc.province},${rec.homeLoc.code},${rec.deadDate},${rec.incidentLoc.district},${rec.incidentLoc.province},${rec.incidentLoc.code},${rec.distanceLevel}\n"
          writer.write(row)
        }

        println(s"Cleaned data with postal codes exported to: $outputFile")
        println(s"Total records: ${dataWithCodes.size}")

      } finally {
        writer.close()
      }

    case Left(error) =>
      System.err.println(s"Error: $error")

def calculateDistanceLevel(homeLoc: Location, incidentLoc: Location): Int =
  val sameProvince = homeLoc.province == incidentLoc.province
  val sameDistrict = homeLoc.district == incidentLoc.district

  if (sameProvince && sameDistrict) 0
  else if (sameProvince && !sameDistrict) 1
  else if (!sameProvince && !sameDistrict) 2
  else -1