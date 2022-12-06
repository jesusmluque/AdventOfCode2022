import scala.annotation.tailrec

object TuningTrouble {

  def countCharsBeforeStartOfPacket(message: String, numberOfInitChars: Int):Int =
    @tailrec
    def countRec(mess:List[Char], acc:Int):Int =
      mess match {
        case m if m.size < numberOfInitChars => 0
        case m if (m.take(numberOfInitChars).toSet intersect m.take(numberOfInitChars).toSet).size == numberOfInitChars => numberOfInitChars + acc
        case a :: rest => countRec(rest, 1 + acc)
      }
    countRec(message.toList, 0)
}
