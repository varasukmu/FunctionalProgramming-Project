import scala.util.Using
import java.io.{PrintWriter, File}
import Models._
import scala.collection.parallel.CollectionConverters._

@main def runAnalysis(args: String*): Unit =
  val mode = args.headOption.getOrElse("sequential").toLowerCase

  mode match
    case "benchmark" => printBenchmark()
    case "parallel"  => reportMode("parallel")
    case _            => reportMode("sequential")

/**
 * Perform work for a single mode and emit output.
 * Side effects are confined here: timing, printing, writing files.
 */
def reportMode(mode: String): Unit =
  val (result, elapsed) = timed(executePipeline(mode))
  result match
    case Right(report) =>
      println(DataReporter.formatReport(report))
      println(f"\n>>> [Mode: $mode] Total Time: $elapsed ms <<<")
    case Left(err) =>
      println(s"Error: $err")

/**
 * Generate benchmark summary string and print it. All computation pure;
 * only the final println calls are effectful.
 */
def printBenchmark(): Unit =
  println("=" * 50)
  println("         SCALA DATA ANALYSIS BENCHMARK")
  println("=" * 50)

  println("1/2 Running Sequential Mode...")
  val (resSeq, timeSeq) = timed(executePipeline("sequential"))

  println("2/2 Running Parallel Mode...")
  val (resPar, timePar) = timed(executePipeline("parallel"))

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

// util helpers ----------------------------------------------------------
def timed[A](block: => A): (A, Long) =
  val start = System.currentTimeMillis()
  val res = block
  val end   = System.currentTimeMillis()
  (res, end - start)

// pipeline --------------------------------------------------------------
def executePipeline(mode: String): Either[String, ProcessData.AnalysisResult] =
  for {
    provinceMap <- LocationMapper.loadProvinceMapping("provinces.csv")
    districtMap <- LocationMapper.loadDistrictMapping("districts.csv")
    data        <- DataExtractor.extractAccidentData("Dataset-Mockup-byGemini.csv")
    // data        <- DataExtractor.extractAccidentData("Dataset.csv")
    dataWithCodes =
      if mode == "parallel" then transformDataParallel(data, provinceMap, districtMap)
      else transformData(data, provinceMap, districtMap)
    csvContent    = toCsvContent(dataWithCodes)
    _            <- writeStringToFile("newDataset.csv", csvContent)
    analysis     <- ProcessData.analyze(dataWithCodes, mode).toRight("Analysis failed")
  } yield analysis

// data transformation --------------------------------------------------
def transformData(
  data: List[AccidentRecord],
  pMap: Map[String, String],
  dMap: Map[String, String]
): Seq[AccidentRecordWithCode] =
  data.map(rec => buildRecord(rec, pMap, dMap))

def transformDataParallel(
  data: List[AccidentRecord],
  pMap: Map[String, String],
  dMap: Map[String, String]
): Seq[AccidentRecordWithCode] =
  data.par.map(rec => buildRecord(rec, pMap, dMap)).seq

def buildRecord(rec: AccidentRecord, pMap: Map[String, String], dMap: Map[String, String]): AccidentRecordWithCode =
  val homeCode = LocationCodeCalculator.getLocationCode(rec.homeLoc, pMap, dMap)
  val incCode  = LocationCodeCalculator.getLocationCode(rec.incidentLoc, pMap, dMap)
  val distLvl  = calculateDistanceLevel(rec.homeLoc, rec.incidentLoc)
  AccidentRecordWithCode(
    rec.age,
    rec.sex,
    LocationWithCode(rec.homeLoc.district, rec.homeLoc.province, homeCode),
    rec.deadDate,
    LocationWithCode(rec.incidentLoc.district, rec.incidentLoc.province, incCode),
    distLvl
  )

// CSV helpers -----------------------------------------------------------
def toCsvContent(data: Seq[AccidentRecordWithCode]): String =
  val header = "Age,Sex,Home_Code,Incident_Code,Distance_Level,day,month,year"
  val rows = data.map { r =>
    // แยกปี-เดือน-วัน จาก deadDate 
    val parts = r.deadDate.trim.split("-")
    val (year, month, day) = if (parts.length == 3) (parts(0), parts(1), parts(2)) else ("","","")
    
    s"${r.age},${r.sex},${r.homeLoc.code},${r.incidentLoc.code},${r.distanceLevel},$day,$month,$year"
  }
  (header +: rows).mkString("\n")

def writeStringToFile(path: String, content: String): Either[String, Unit] =
  Using(new PrintWriter(new File(path))) { pw => pw.print(content) }
    .toEither
    .left
    .map(_.getMessage)

// utility ---------------------------------------------------------------
def calculateDistanceLevel(home: Location, incident: Location): Int =
  (home.province == incident.province, home.district == incident.district) match
    case (true, true)   => 0
    case (true, false)  => 1
    case (false, _)     => 2