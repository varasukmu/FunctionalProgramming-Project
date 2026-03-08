import scala.util.{Using, Try}
import java.io.{PrintWriter, File}
import Models._
import scala.collection.parallel.CollectionConverters._

@main def runAnalysis(args: String*): Unit =
  val mode = args.headOption.getOrElse("sequential").toLowerCase
  
  mode match
    case "benchmark" => runBenchmarkMode()
    case "parallel"  => runSingleMode("parallel")
    case _           => runSingleMode("sequential")

def runSingleMode(mode: String): Unit =
  val startTime = System.currentTimeMillis()
  executePipeline(mode) match
    case Right(report) => 
      DataReporter.printReport(report)
      val endTime = System.currentTimeMillis()
      println(f"\n>>> [Mode: $mode] Total Time: ${endTime - startTime} ms <<<")
    case Left(err) => 
      println(s"Error: $err")

def runBenchmarkMode(): Unit =
  println("=" * 50)
  println("         SCALA DATA ANALYSIS BENCHMARK")
  println("=" * 50)
  
  println("1/2 Running Sequential Mode...")
  val startSeq = System.currentTimeMillis()
  val resSeq = executePipeline("sequential")
  val timeSeq = System.currentTimeMillis() - startSeq

  println("2/2 Running Parallel Mode...")
  val startPar = System.currentTimeMillis()
  val resPar = executePipeline("parallel")
  val timePar = System.currentTimeMillis() - startPar

  println("\n" + "=" * 50)
  println(f"| ${"Mode"}%-15s | ${"Execution Time (ms)"}%-25s |")
  println("-" * 50)
  println(f"| ${"Sequential"}%-15s | $timeSeq%-25d |")
  println(f"| ${"Parallel"}%-15s | $timePar%-25d |")
  println("-" * 50)
  
  if (timePar > 0) then
    val speedup = timeSeq.toDouble / timePar
    println(f"Speedup Factor: $speedup%.2fx faster")
  
  println("=" * 50)

// --- Pipeline การประมวลผลหลัก ---
def executePipeline(mode: String): Either[String, ProcessData.AnalysisResult] =
  val provinceMap = LocationMapper.loadProvinceMapping("provinces.csv")
  val districtMap = LocationMapper.loadDistrictMapping("districts.csv")
  
  for {
    data <- DataExtractor.extractAccidentData("Dataset.csv")
    dataWithCodes = if (mode == "parallel") then transformDataParallel(data, provinceMap, districtMap) 
                    else transformData(data, provinceMap, districtMap)
    _ <- exportToCsv(dataWithCodes, "newDataset.csv")
    analysis <- ProcessData.analyze(dataWithCodes, mode).toRight("Analysis failed")
  } yield analysis

// --- ฟังก์ชันจัดการข้อมูล (ระวังอย่าให้ขาดส่วนท้าย!) ---
def transformData(data: List[AccidentRecord], pMap: Map[String, String], dMap: Map[String, String]): List[AccidentRecordWithCode] =
  data.map(rec => buildRecord(rec, pMap, dMap))

def transformDataParallel(data: List[AccidentRecord], pMap: Map[String, String], dMap: Map[String, String]): List[AccidentRecordWithCode] =
  data.par.map(rec => buildRecord(rec, pMap, dMap)).toList

def buildRecord(rec: AccidentRecord, pMap: Map[String, String], dMap: Map[String, String]): AccidentRecordWithCode =
  val homeCode = LocationCodeCalculator.getLocationCode(rec.homeLoc, pMap, dMap)
  val incCode = LocationCodeCalculator.getLocationCode(rec.incidentLoc, pMap, dMap)
  val distLvl = calculateDistanceLevel(rec.homeLoc, rec.incidentLoc)
  AccidentRecordWithCode(rec.age, rec.sex, LocationWithCode(rec.homeLoc.district, rec.homeLoc.province, homeCode), rec.deadDate, LocationWithCode(rec.incidentLoc.district, rec.incidentLoc.province, incCode), distLvl)

def exportToCsv(data: List[AccidentRecordWithCode], path: String): Either[String, Unit] =
  Using(new PrintWriter(new File(path))) { writer =>
    writer.println("Age,Sex,Home_Code,Incident_Code,Distance_Level")
    data.foreach(r => writer.println(s"${r.age},${r.sex},${r.homeLoc.code},${r.incidentLoc.code},${r.distanceLevel}"))
  }.toEither.left.map(_.getMessage)

def calculateDistanceLevel(home: Location, incident: Location): Int =
  (home.province == incident.province, home.district == incident.district) match
    case (true, true)   => 0
    case (true, false)  => 1
    case (false, _)     => 2