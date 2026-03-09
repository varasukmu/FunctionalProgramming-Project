import Models.AccidentRecordWithCode
import ProcessData.{AnalysisResult, calculateStats} 

object DataReporter:
  def formatReport(result: AnalysisResult): String =
    val sb = new StringBuilder

    def appendLine(line: String = ""): Unit = sb.append(line).append("\n")

    def printSummaryStats(total: Int,processed: Int, avgAge: Double, ageStdDev: Double,
      ageBins: Seq[(String, Int, Double)], sexStats: Map[String, (Int, Double)], distStats: Map[Int, (Int, Double)]): Unit =
      val levelDesc = Map(0 -> "จังหวัดและอำเภอเดียวกัน", 1 -> "จังหวัดเดียวกัน คนละอำเภอ", 
        2 -> "คนละอำเภอ และจังหวัด",-1 -> "ข้อมูลไม่สอดคล้อง/อื่นๆ")

      appendLine("=" * 65)
      appendLine("\tSummary Statistics")
      appendLine("=" * 65)
      
      appendLine(f"Total Rows Received  : $total%,d records")
      appendLine(f"Total Rows Processed : $processed%,d records (Valid Data)")
      appendLine("-" * 65)
      
      appendLine(f"\nAverage Age:\n\t$avgAge%.2f years (SD: $ageStdDev%.2f)")
      appendLine("\nAge Demographics (Bins):")
      ageBins.foreach { case (label, count, pct) =>
        appendLine(f"\t- Age $label%-5s : $pct%5.2f%% ($count records)")
      }
      appendLine("\nSex (Overall):")
      sexStats.toSeq.sortBy { case (_, (count, _)) => -count }.foreach { case (sex, (count, pct)) =>
        val label = if sex.trim.isEmpty then "Unknown" else sex
        appendLine(f"\t- $label%-9s : $pct%5.2f%% ($count records)")
      }
      appendLine("\nDistance Level (Overall):")
      distStats.toSeq.sortBy(_._1).foreach { case (level, (count, pct)) =>
        val desc = levelDesc.getOrElse(level, "Unknown")
        appendLine(f"\t- Level $level ($desc): $pct%.2f%% ($count records)")
      }

    def printTopSexAndAgeGroups(topGroups: Seq[((String, String), Int, Double)]): Unit =
      appendLine("\n" + "-" * 65)
      appendLine(s"\tTop ${ProcessData.ShowRank} Incident Groups (Sex & Age)")
      appendLine("-" * 65)
      appendLine()
      topGroups.zipWithIndex.foreach { case (((sex, ageBin), count, pct), index) =>
        appendLine(f"${index + 1}%2d. เพศ: $sex%-8s | ช่วงอายุ: $ageBin%-5s ปี : $pct%5.2f%% ($count records)")
      }

    def printTopProvincesDetails(topProvinces: Seq[(String, List[AccidentRecordWithCode])], totalOriginalRecords: Int): Unit =
      appendLine("\n" + "-" * 65)
      appendLine(s"\tTop ${ProcessData.ShowRank} Incident Provinces (Detailed View)")
      appendLine("-" * 65)
      appendLine()
      topProvinces.zipWithIndex.foreach { case ((prov, provinceRecords), index) =>
        val provTotal = provinceRecords.size
        val provOverallPct = (provTotal.toDouble / totalOriginalRecords) * 100
        appendLine(f"${index + 1}. จ.$prov : $provOverallPct%.2f%% ($provTotal)")

        val sexStats = calculateStats(provinceRecords, provTotal, "sequential")(_.sex)
        val sexStrings = sexStats.toSeq
          .sortBy { case (_, (count, _)) =>
            -count
          }
          .map { case (sex, (count, pct)) =>
            val sName = if sex.trim.isEmpty 
            then "Unknown"
            else sex
            f"sex $sName : $pct%.2f%% ($count)"
          }
        appendLine(s"\t${sexStrings.mkString(" | ")}")

        val distLevelStats = calculateStats(provinceRecords, provTotal, "sequential")(_.distanceLevel)
        val levelStrings = Seq(0, 1, 2).map { lvl =>
          val (count, pct) = distLevelStats.getOrElse(lvl, (0, 0.0))
          f"level $lvl : $pct%.2f%% ($count)"
        }
        appendLine(s"\t${levelStrings.mkString(" | ")}")
        appendLine()
      }

    def printTopLocationsDetails(topLocations: Seq[((String, String), List[AccidentRecordWithCode])], totalOriginalRecords: Int): Unit =
      appendLine("-" * 65)
      appendLine(s"\tTop ${ProcessData.ShowRank} Incident Locations (Detailed View)")
      appendLine("-" * 65)
      appendLine()
      topLocations.zipWithIndex.foreach { case (((prov, dist), districtRecords), index) =>
        val distTotal = districtRecords.size
        val distOverallPct = (distTotal.toDouble / totalOriginalRecords) * 100
        appendLine(f"${index + 1}. อ.$dist จ.$prov : $distOverallPct%.2f%% ($distTotal)")

        val sexStats = calculateStats(districtRecords, distTotal, "sequential")(_.sex)
        val sexStrings = sexStats.toSeq
          .sortBy { case (_, (count, _)) => -count }
          .map { case (sex, (count, pct)) =>
            val sName = if sex.trim.isEmpty then "Unknown" else sex
            f"sex $sName : $pct%.2f%% ($count)"
          }
        appendLine(s"\t${sexStrings.mkString(" | ")}")

        val distLevelStats = calculateStats(districtRecords, distTotal, "sequential")(_.distanceLevel)
        val levelStrings = Seq(0, 1, 2).map { lvl =>
          val (count, pct) = distLevelStats.getOrElse(lvl, (0, 0.0))
          f"level $lvl : $pct%.2f%% ($count)"
        }
        appendLine(s"\t${levelStrings.mkString(" | ")}")
        appendLine()
      }

    def printTopMonths(topMonths: Seq[(Int, Int)], totalOriginalRecords: Int): Unit =
      appendLine("-" * 65)
      appendLine(s"\tTop ${ProcessData.ShowRank} Incident Months")
      appendLine("-" * 65)
      appendLine()
      val monthNames = Map(
        1 -> "มกราคม", 2 -> "กุมภาพันธ์", 3 -> "มีนาคม", 4 -> "เมษายน",
        5 -> "พฤษภาคม", 6 -> "มิถุนายน", 7 -> "กรกฎาคม", 8 -> "สิงหาคม",
        9 -> "กันยายน", 10 -> "ตุลาคม", 11 -> "พฤศจิกายน", 12 -> "ธันวาคม"
      )
      topMonths.zipWithIndex.foreach { case ((month, count), index) =>
        val mName = monthNames.getOrElse(month, s"เดือนที่ $month")
        val pct = (count.toDouble / totalOriginalRecords) * 100
        appendLine(f"${index + 1}. $mName%-15s : $pct%5.2f%% ($count records)")
      }
      appendLine()
      appendLine("=" * 65)

    printSummaryStats( result.total, result.processedRows, result.avgAge, result.ageStdDev,
      result.ageBins, result.sexStats, result.distStats)
    printTopSexAndAgeGroups(result.topSexAndAgeGroups)
    printTopProvincesDetails(result.topProvinces, result.total)
    printTopLocationsDetails(result.topLocations, result.total)
    printTopMonths(result.topMonths, result.total)

    sb.toString