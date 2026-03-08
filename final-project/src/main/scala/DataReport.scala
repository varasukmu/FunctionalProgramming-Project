import Models.AccidentRecordWithCode
import ProcessData.{AnalysisResult, calculateStats} 

object DataReporter:

  def printReport(result: AnalysisResult): Unit =
    printSummaryStats(result.total, result.avgAge, result.ageStdDev, result.ageBins, result.sexStats, result.distStats)
    printTopSexAndAgeGroups(result.topSexAndAgeGroups) // <- เพิ่มตรงนี้
    printTopProvincesDetails(result.topProvinces, result.total)
    printTopLocationsDetails(result.topLocations, result.total)

  private def printSummaryStats(
    total: Int, 
    avgAge: Double, 
    ageStdDev: Double,
    ageBins: Seq[(String, Int, Double)],
    sexStats: Map[String, (Int, Double)], 
    distStats: Map[Int, (Int, Double)]
  ): Unit =
    val levelDesc = Map(
      0 -> "จังหวัดและอำเภอเดียวกัน",
      1 -> "จังหวัดเดียวกัน คนละอำเภอ",
      2 -> "คนละอำเภอ และจังหวัด",
      -1 -> "ข้อมูลไม่สอดคล้อง/อื่นๆ"
    )

    println("=" * 65)
    println("\tSummary Statistics")
    println("=" * 65)
    
    println(f"\nAverage Age:\n\t$avgAge%.2f years (SD: $ageStdDev%.2f)") 
    
    println("\nAge Demographics (Bins):")
    ageBins.foreach { case (label, count, pct) =>
      println(f"\t- Age $label%-5s : $pct%5.2f%% ($count records)")
    }

    println("\nSex Breakdown (Overall):")
    sexStats.toSeq.sortBy { case (_, (count, _)) => -count }.foreach { case (sex, (count, pct)) => 
      val label = if sex.trim.isEmpty then "Unknown" else sex
      println(f"\t- $label%-9s : $pct%5.2f%% ($count records)") 
    }

    println("\nDistance Level (Overall):")
    distStats.toSeq.sortBy(_._1).foreach { case (level, (count, pct)) => 
      val desc = levelDesc.getOrElse(level, "Unknown")
      println(f"\t- Level $level ($desc): $pct%.2f%% ($count records)") 
    }

  private def printTopSexAndAgeGroups(topGroups: Seq[((String, String), Int, Double)]): Unit =
    println("\n" + "-" * 65)
    println("\tTop 10 Incident Groups (Sex & Age)")
    println("-" * 65)
    println("")

    topGroups.zipWithIndex.foreach { case (((sex, ageBin), count, pct), index) =>
      println(f"${index + 1}%2d. เพศ: $sex%-8s | ช่วงอายุ: $ageBin%-5s ปี : $pct%5.2f%% ($count records)")
    }

  private def printTopProvincesDetails(topProvinces: Seq[(String, List[AccidentRecordWithCode])], totalOriginalRecords: Int): Unit =
    println("\n" + "-" * 65)
    println("\tTop 10 Incident Provinces (Detailed View)")
    println("-" * 65)
    println("")

    topProvinces.zipWithIndex.foreach { case ((prov, provinceRecords), index) =>
      val provTotal = provinceRecords.size
      val provOverallPct = (provTotal.toDouble / totalOriginalRecords) * 100 
      
      println(f"${index + 1}. จ.$prov : $provOverallPct%.2f%% ($provTotal)")

      val sexStats = calculateStats(provinceRecords, provTotal)(_.sex)
      val sexStrings = sexStats.toSeq.sortBy { case (_, (count, _)) => -count }.map { case (sex, (count, pct)) =>
        val sName = if sex.trim.isEmpty then "Unknown" else sex
        f"sex $sName : $pct%.2f%% ($count)"
      }
      println(s"\t${sexStrings.mkString(" | ")}")

      val distLevelStats = calculateStats(provinceRecords, provTotal)(_.distanceLevel)
      val levelStrings = Seq(0, 1, 2).map { lvl =>
        val (count, pct) = distLevelStats.getOrElse(lvl, (0, 0.0))
        f"level $lvl : $pct%.2f%% ($count)"
      }
      println(s"\t${levelStrings.mkString(" | ")}")
      println() 
    }

  private def printTopLocationsDetails(topLocations: Seq[((String, String), List[AccidentRecordWithCode])], totalOriginalRecords: Int): Unit =
    println("-" * 65)
    println("\tTop 10 Incident Locations (Detailed View)")
    println("-" * 65)
    println("")

    topLocations.zipWithIndex.foreach { case (((prov, dist), districtRecords), index) =>
      val distTotal = districtRecords.size
      val distOverallPct = (distTotal.toDouble / totalOriginalRecords) * 100
      
      println(f"${index + 1}. อ.$dist จ.$prov : $distOverallPct%.2f%% ($distTotal)")

      val sexStats = calculateStats(districtRecords, distTotal)(_.sex)
      val sexStrings = sexStats.toSeq.sortBy { case (_, (count, _)) => -count }.map { case (sex, (count, pct)) =>
        val sName = if sex.trim.isEmpty then "Unknown" else sex
        f"sex $sName : $pct%.2f%% ($count)"
      }
      println(s"\t${sexStrings.mkString(" | ")}")

      val distLevelStats = calculateStats(districtRecords, distTotal)(_.distanceLevel)
      val levelStrings = Seq(0, 1, 2).map { lvl =>
        val (count, pct) = distLevelStats.getOrElse(lvl, (0, 0.0))
        f"level $lvl : $pct%.2f%% ($count)"
      }
      println(s"\t${levelStrings.mkString(" | ")}")
      println() 
    }
    println("=" * 65)