import scala.annotation.tailrec

object RockPaperScissors {

  def getScore(rawStrategy: List[String]) =
    @tailrec
    def getScoreTailRec(rawStrategy: List[String], acc: Long):Long =  rawStrategy match
      case "A X" :: r:List[String] => getScoreTailRec(r, 4 + acc)
      case "A Y" :: r:List[String] => getScoreTailRec(r, 8 + acc)
      case "A Z" :: r:List[String] => getScoreTailRec(r, 3 + acc)
      case "B X" :: r:List[String] => getScoreTailRec(r, 1 + acc)
      case "B Y" :: r:List[String] => getScoreTailRec(r, 5 + acc)
      case "B Z" :: r:List[String] => getScoreTailRec(r, 9 + acc)
      case "C X" :: r:List[String] => getScoreTailRec(r, 7 + acc)
      case "C Y" :: r:List[String] => getScoreTailRec(r, 2 + acc)
      case "C Z" :: r:List[String] => getScoreTailRec(r, 6 + acc)
      case _  => acc

    getScoreTailRec(rawStrategy, 0)

  def getScoreSecondStrategy(rawStrategy: List[String]): Long =
    val newStrategy = rawStrategy.map {
      case "A X" => "A Z"
      case "A Y" => "A X"
      case "A Z" => "A Y"
      case "B X" => "B X"
      case "B Y" => "B Y"
      case "B Z" => "B Z"
      case "C X" => "C Y"
      case "C Y" => "C Z"
      case "C Z" => "C X"
    }
    getScore(newStrategy)
}
