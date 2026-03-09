import Models.{AccidentRecordWithCode, AccidentWithDate}
import scala.util.Try
import scala.collection.parallel.CollectionConverters._

object ProcessData:

  val ShowRank: Int = 3 

  case class AnalysisResult(
    total: Int,
    processedRows: Int, // ---> จุดที่ 1: เพิ่มฟิลด์รับค่าจำนวนที่ประมวลผลผ่าน
    avgAge: Double,
    ageStdDev: Double,
    ageBins: Seq[(String, Int, Double)], 
    sexStats: Map[String, (Int, Double)],
    distStats: Map[Int, (Int, Double)],
    topSexAndAgeGroups: Seq[((String, String), Int, Double)],
    topProvinces: Seq[(String, List[AccidentRecordWithCode])],
    topLocations: Seq[((String, String), List[AccidentRecordWithCode])],
    topMonths: Seq[(Int, Int)]
  )

  def analyze(data: List[AccidentRecordWithCode], mode: String): Option[AnalysisResult] =
    val total = data.size
    if (total == 0) None
    else
      val (avgAge, stdDev, ageBins) = calculateAgeDemographics(data)
      
      val dataWithDates = data.flatMap(splitDate)
      val processedCount = dataWithDates.size // ---> จุดที่ 2: นับจำนวนข้อมูลที่มีวันที่ถูกต้อง
      
      val (topProv, topLoc, topM) = if (mode == "parallel") then
        val p = data.par.groupBy(_.incidentLoc.province)
                  .mapValues(_.toList).seq.toSeq
                  .sortBy(-_._2.size).take(ShowRank).toList
        val l = data.par.groupBy(r => (r.incidentLoc.province, r.incidentLoc.district))
                  .mapValues(_.toList).seq.toSeq
                  .sortBy(-_._2.size).take(ShowRank).toList
                  
        val m = dataWithDates.par.groupBy(_.month)
                  .mapValues(_.size).seq.toSeq
                  .sortBy(-_._2).take(ShowRank).toList
        (p, l, m)
      else
        val p = data.groupBy(_.incidentLoc.province).toSeq.sortBy(-_._2.size).take(ShowRank).toList
        val l = data.groupBy(r => (r.incidentLoc.province, r.incidentLoc.district)).toSeq.sortBy(-_._2.size).take(ShowRank).toList
        
        val m = dataWithDates.groupBy(_.month).view.mapValues(_.size).toSeq.sortBy(-_._2).take(ShowRank).toList
        (p, l, m)

      Some(AnalysisResult(
        total = total,
        processedRows = processedCount, // ---> จุดที่ 3: ส่งค่าเข้าไปใน Result
        avgAge = avgAge,
        ageStdDev = stdDev,
        ageBins = ageBins,
        sexStats = calculateStats(data, total)(_.sex),
        distStats = calculateStats(data, total)(_.distanceLevel),
        topSexAndAgeGroups = getTopSexAndAgeGroups(data, total, ShowRank),
        topProvinces = topProv,
        topLocations = topLoc,
        topMonths = topM
      ))

  private def splitDate(rec: AccidentRecordWithCode): Option[AccidentWithDate] =
    rec.deadDate.trim.split("-") match
      case Array(year, month, day) =>
        Try {
          AccidentWithDate(
            rec.age,
            rec.sex,
            rec.homeLoc,
            rec.incidentLoc,
            rec.distanceLevel,
            day.toInt,
            month.toInt,
            year.toInt
          )
        }.toOption
      case _ => None

  def calculateStats[T](data: List[AccidentRecordWithCode], total: Int)(f: AccidentRecordWithCode => T): Map[T, (Int, Double)] =
    if (total == 0) Map.empty
    else data.groupBy(f).view.mapValues { list => 
      val count = list.size
      (count, (count.toDouble / total) * 100)
    }.toMap

  private def calculateAgeDemographics(data: List[AccidentRecordWithCode]): (Double, Double, Seq[(String, Int, Double)]) =
    val validAges = data.flatMap(r => Try(r.age.toInt).toOption).filter(_ >= 0)
    val totalValid = validAges.size
    if (totalValid == 0) (0.0, 0.0, Seq.empty)
    else 
      val mean = validAges.sum.toDouble / totalValid
      val stdDev = math.sqrt(validAges.map(a => math.pow(a - mean, 2)).sum / totalValid)
      val bins = validAges.map(getAgeBinLabel).groupBy(identity).view.mapValues(_.size).toSeq
        .sortBy { case (label, _) => if (label == "80+") 999 else Try(label.split("-")(0).toInt).getOrElse(0) }
        .map { case (label, count) => (label, count, (count.toDouble / totalValid) * 100) }
      (mean, stdDev, bins.toSeq)

  private def getTopSexAndAgeGroups(data: List[AccidentRecordWithCode], total: Int, limit: Int): Seq[((String, String), Int, Double)] =
    if (total == 0) Seq.empty
    else data.flatMap { r => 
      Try(r.age.toInt).toOption.filter(_ >= 0).map(age => 
        (if (r.sex.trim.isEmpty) "Unknown" else r.sex, getAgeBinLabel(age))
      )
    }.groupBy(identity).view.mapValues { records => 
      val count = records.size; (count, (count.toDouble / total) * 100) 
    }.toSeq.sortBy { case (_, (count, _)) => -count }.take(limit).map { case (key, (count, pct)) => (key, count, pct) }

  private def getAgeBinLabel(age: Int): String =
    val startAge = if age >= 80 then 80 else (age / 10) * 10
    if startAge == 80 then "80+" else s"$startAge-${startAge + 9}"