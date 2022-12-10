object RopeBridge:

  case class Instruction(direction: String, steps: Int)
  object Instruction:
    private val pattern = "([RLUD]{1}) ([0-9]+)".r
    def apply(rawInstruction: String):Instruction = rawInstruction match
      case pattern(d, s) => Instruction(d, s.toInt)

  case class Point(x: Int, y: Int):
    def nextPoints(i: Instruction) =
      val Instruction(d, s) = i
      (1 to s).foldLeft(List[Point](this)) { (acc, _) =>
        d match {
          case "R" => Point(acc.head.x + 1, acc.head.y) :: acc
          case "L" => Point(acc.head.x - 1, acc.head.y) :: acc
          case "U" => Point(acc.head.x, acc.head.y + 1) :: acc
          case "D" => Point(acc.head.x, acc.head.y - 1) :: acc
        }
      }.reverse

  def countPointsVisitedByTailIn(rawInstructions: List[String]) =
    val headPositionHistory = calculateHeadPositionHistoryFrom(rawInstructions)
    getPositionsHistoryFromPreviousHead(headPositionHistory).toSet.size

  def countPointsVisitedByLongTailIn(rawInstructions: List[String]) =
    val headPositionHistory = calculateHeadPositionHistoryFrom(rawInstructions)
    val r1 = getPositionsHistoryFromPreviousHead(headPositionHistory)
    val r2 = getPositionsHistoryFromPreviousHead(r1)
    val r3 = getPositionsHistoryFromPreviousHead(r2)
    val r4 = getPositionsHistoryFromPreviousHead(r3)
    val r5 = getPositionsHistoryFromPreviousHead(r4)
    val r6 = getPositionsHistoryFromPreviousHead(r5)
    val r7 = getPositionsHistoryFromPreviousHead(r6)
    val r8 = getPositionsHistoryFromPreviousHead(r7)
    getPositionsHistoryFromPreviousHead(r8).toSet.size

  private def calculateHeadPositionHistoryFrom(rawInstructions: List[String]) = {
    val headPositionHistory = rawInstructions.foldLeft((List[Point](), Point(0, 0))) { (acc, ins) =>
      val nextPositions = acc._2.nextPoints(Instruction(ins)).reverse
      (nextPositions ++ acc._1, nextPositions.head)
    }._1.reverse
    headPositionHistory
  }

  private def getPositionsHistoryFromPreviousHead(headHistory: List[Point]) =
    headHistory.foldLeft((List[Point](), Point(0, 0))) { (acc, p) =>
      val newTailPos = newPositionFromPreviousHead(p, acc._2)
      (newTailPos :: acc._1, newTailPos)
    }._1.reverse

  private def newPositionFromPreviousHead(newHead: Point, oldTail: Point):Point =
    (newHead.x - oldTail.x, newHead.y - oldTail.y) match {
      case (0, 0) => oldTail
      case (1, 0) => oldTail
      case (0, 1) => oldTail
      case (0, -1) => oldTail
      case (-1, 0) => oldTail
      case (-1, -1) => oldTail
      case (1, -1) => oldTail
      case (-1, 1) => oldTail
      case (1, 1) => oldTail
      case (a, 0) if a > 1 => Point(a - 1 + oldTail.x, oldTail.y)
      case (0, b) if b > 1 => Point(oldTail.x, b - 1 + oldTail.y)
      case (a, 1) if a > 1 => Point(a - 1 + oldTail.x, 1 + oldTail.y)
      case (1, b) if b > 1 => Point(1 + oldTail.x, b - 1 + oldTail.y)
      case (-1, b) if b > 1 => Point(-1 + oldTail.x, b - 1 + oldTail.y)
      case (a, 0) if a < -1 => Point(a + 1 + oldTail.x, oldTail.y)
      case (0, b) if b < -1 => Point(oldTail.x, b + 1 + oldTail.y)
      case (a, 1) if a < -1 => Point(a + 1 + oldTail.x, 1 + oldTail.y)
      case (a, -1) if a < -1 => Point(a + 1 + oldTail.x, -1 + oldTail.y)
      case (a, -1) if a > 1 => Point(a - 1 + oldTail.x, -1 + oldTail.y)
      case (1, b) if b < -1 => Point(1 + oldTail.x, b + 1 + oldTail.y)
      case (-1, b) if b < -1 => Point(-1 + oldTail.x, b + 1 + oldTail.y)
      case (2,2) => Point(1 + oldTail.x, 1 + oldTail.y)
      case (-2,2) => Point(-1 + oldTail.x, 1 + oldTail.y)
      case (-2,-2) => Point(-1 + oldTail.x, -1 + oldTail.y)
      case (2,-2) => Point(1 + oldTail.x, -1 + oldTail.y)
    }