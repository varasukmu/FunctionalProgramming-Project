import scala.io.Source
import scala.util.{Using, Try}

import Models.{Location, AccidentRecord}

object DataExtractor {
  def extractAccidentData(fileName: String): Either[String, List[AccidentRecord]] =
    Using(Source.fromFile(fileName, "UTF-8")) { source =>
      val allLines = source.getLines().toList

      allLines match
        case Nil => Nil
        case headerLine :: dataLines =>
          val header = headerLine.split(",").map(_.trim).toList
          val idx = (name: String) => header.indexOf(name)

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

            def get(key: String): Option[String] =
              colsMap.get(key).filter(i => i >= 0 && i < c.length && c(i).nonEmpty).map(c(_))

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
}