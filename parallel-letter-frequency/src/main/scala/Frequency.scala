import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Frequency {
  def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] = {
    val chars = texts.flatten.filter(_.isLetter).map(_.toLower)
    val workChunks = chars.grouped(1 max chars.length / numWorkers).toList
    val charCountListsF = workChunks.map(charMapAsync)
    val charCountMapF = Future.foldLeft(charCountListsF)(Map.empty[Char, Int]){ case (charCountMap, y) =>
      y.foldLeft(charCountMap){ case (charIntMap, (c, count)) =>
        charIntMap.updatedWith(c) {
          case Some(acc) => Some(acc + count)
          case None => Some(count)
        }
      }
    }

    Await.result(charCountMapF, 10.seconds)
  }

  def charMapAsync(chars: Seq[Char]): Future[List[(Char, Int)]] = Future {
    chars.groupBy(identity).mapValues(_.length).toList
  }
}
