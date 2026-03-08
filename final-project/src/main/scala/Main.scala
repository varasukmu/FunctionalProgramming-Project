@main def runAnalysis(): Unit =
  val fileName = "Dataset.csv" // กำหนดชื่อไฟล์ไ 
  println(s"--- Starting Functional Analysis on: $fileName ---")

  extractAccidentData(fileName) match
    case Right(data) =>
      println(s"Success! Found ${data.size} clean records.")

      // เรียก function แยกวันเดือนปี
      val dateprocessed =
        data.flatMap(DataProcessor.splitDate)

      // data.take(100).foreach { rec =>
      //   println(s"Age: ${rec.age} | Sex: ${rec.sex} | Resident: ${rec.homeLoc} | Scene: ${rec.incidentLoc} | Date: ${rec.deadDate}")
      // }
      
      dateprocessed.take(100).foreach { rec =>
        println(s"Age: ${rec.age} | Sex: ${rec.sex} | Resident: ${rec.homeLoc} | Scene: ${rec.incidentLoc} | Date: ${rec.deadDate} | Day: ${rec.day} | Month: ${rec.month} | Year: ${rec.year}")
      }

      println(s"Records with valid date: ${dateprocessed.size}")

    case Left(error) => 
      System.err.println(error)