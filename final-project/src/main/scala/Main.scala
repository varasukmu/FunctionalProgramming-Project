import scala.util.{Using, Try}
import java.io.{PrintWriter, File}
import Models._

@main def runAnalysis(): Unit =
  val provinceMap = LocationMapper.loadProvinceMapping("provinces.csv")
  val districtMap = LocationMapper.loadDistrictMapping("districts.csv")

  val result = for {
    data <- DataExtractor.extractAccidentData("Dataset.csv")
    dataWithCodes = transformData(data, provinceMap, districtMap)
    _ <- exportToCsv(dataWithCodes, "newDataset.csv")
    analysis <- ProcessData.analyze(dataWithCodes).toRight("Analysis failed")
  } yield analysis

  result match
    case Right(report) => DataReporter.printReport(report)
    case Left(err)     => println(s"Error: $err")

def transformData(data: List[AccidentRecord], pMap: Map[String, String], dMap: Map[String, String]): List[AccidentRecordWithCode] =
  data.map { rec =>
    val homeCode = LocationCodeCalculator.getLocationCode(rec.homeLoc, pMap, dMap)
    val incCode = LocationCodeCalculator.getLocationCode(rec.incidentLoc, pMap, dMap)
    val distLvl = calculateDistanceLevel(rec.homeLoc, rec.incidentLoc)
    
    AccidentRecordWithCode(
      rec.age, rec.sex, 
      LocationWithCode(rec.homeLoc.district, rec.homeLoc.province, homeCode),
      rec.deadDate,
      LocationWithCode(rec.incidentLoc.district, rec.incidentLoc.province, incCode),
      distLvl
    )
  }

def exportToCsv(data: List[AccidentRecordWithCode], path: String): Either[String, Unit] =
  Using(new PrintWriter(new File(path))) { writer =>
    writer.println("Age,Sex,Home_Code,Incident_Code,Distance_Level")
    data.foreach { r =>
      writer.println(s"${r.age},${r.sex},${r.homeLoc.code},${r.incidentLoc.code},${r.distanceLevel}")
    }
  }.toEither.left.map(_.getMessage)

def calculateDistanceLevel(home: Location, incident: Location): Int =
  (home.province == incident.province, home.district == incident.district) match
    case (true, true)   => 0
    case (true, false)  => 1
    case (false, _)     => 2