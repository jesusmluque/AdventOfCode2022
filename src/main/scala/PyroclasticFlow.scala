import scala.annotation.tailrec

object PyroclasticFlow {

  case class Rock(rockType: String, currentPosition: Vector[(Long,Long)])
  case class State(grid: Set[(Long,Long)], rock: Rock, offSet:Int)
  case class Game(until: Long):
    private val STARTING_HEIGHT = 4
    private val STARTING_LEFT = 2
    private val WIDE = 7
    private val IH = Rock("IH", Vector((0,0),(0,1),(0,2),(0,3)))
    private val P = Rock("P", Vector((0,1),(1,0),(1,1),(1,2),(2,1)))
    private val L = Rock("L", Vector((0,0),(0,1),(0,2),(1,2),(2,2)))
    private val IV = Rock("IV", Vector((0,0),(1,0),(2,0),(3,0)))
    private val S = Rock("S", Vector((0,0),(0,1),(1,0),(1,1)))

    def nextRock(currentRock: Rock) = currentRock match
      case Rock("IH", _) => P
      case Rock("P", _) => L
      case Rock("L", _) => IV
      case Rock("IV", _) => S
      case Rock("S", _) => IH

    def moveRight(rock: Rock, g: Set[(Long,Long)], step:Int = 1) =
      val maxR = rock.currentPosition.maxBy(_._2)
      val newPos = rock.currentPosition.foldLeft(Vector[(Long,Long)]())((a, p) => a :+ (if maxR._2 + step < WIDE then (p._1, p._2 + step) else (p._1, p._2)))
      val isNewPosValid = newPos.forall(p => !g.contains(p))
      if isNewPosValid then Rock(rock.rockType, newPos) else rock

    def moveLeft(rock: Rock, g: Set[(Long,Long)]) =
      val minL = rock.currentPosition.minBy(_._2)
      val newPos = rock.currentPosition.foldLeft(Vector[(Long,Long)]())((a, p) => a :+ (if minL._2 - 1 >= 0 then (p._1, p._2 - 1) else (p._1, p._2)))
      val isNewPosValid = newPos.forall(p => !g.contains(p))
      if isNewPosValid then Rock(rock.rockType,newPos) else rock

    def moveDown(rock: Rock, g: Set[(Long,Long)]) =
      val minV = rock.currentPosition.minBy(_._1)
      val newPos = rock.currentPosition.foldLeft(Vector[(Long,Long)]())((a, p) => a :+ (if minV._1 > 0 then (p._1 - 1, p._2) else (p._1, p._2)))
      val isNewPosValid = newPos.forall(p => !g.contains(p))
      if isNewPosValid then Rock(rock.rockType, newPos) else rock

    def initRockPosition(rock: Rock, offset: Long = 0L) =
      rock match
        case Rock("IH", _) => Rock("IH", IH.currentPosition.map(p => (p._1 + STARTING_HEIGHT + offset, p._2 + STARTING_LEFT)))
        case Rock("P", _) => Rock("P", P.currentPosition.map(p => (p._1 + STARTING_HEIGHT + offset, p._2 + STARTING_LEFT)))
        case Rock("IV", _) => Rock("IV", IV.currentPosition.map(p => (p._1 + STARTING_HEIGHT + offset, p._2 + STARTING_LEFT)))
        case Rock("L", _) => Rock("L", L.currentPosition.map(p => (p._1 + STARTING_HEIGHT + offset, p._2 + STARTING_LEFT)))
        case Rock("S", _) => Rock("S", S.currentPosition.map(p => (p._1 + STARTING_HEIGHT + offset, p._2 + STARTING_LEFT)))

    def play(jetPattern: String) =
      def applyJetPattern(st: State, dir: String) =
        st match {
          case State(g, r, _) if dir == ">" => State(g, moveRight(r, g), st.offSet)
          case State(g, r, _) if dir == "<" => State(g, moveLeft(r, g), st.offSet)
        }
      @tailrec
      def playRec(st: State, pattern: List[String], until: Long, rocks: List[Rock]): State =
        if st.offSet == until then st
        else
          val dir = pattern.head
          val firstMovementS = applyJetPattern(st, dir)
          val newDownPos = moveDown(firstMovementS.rock, firstMovementS.grid)
          if newDownPos == firstMovementS.rock then
            val newSet = firstMovementS.grid ++ newDownPos.currentPosition.toSet
            val nextHighestPoint = newSet.maxBy(_._1)._1
            playRec(State(newSet, initRockPosition(nextRock(firstMovementS.rock), nextHighestPoint), st.offSet + 1), if pattern.tail.isEmpty then jetPattern.split("").toList else pattern.tail, until, newDownPos :: rocks)
          else
            playRec(State(firstMovementS.grid, newDownPos, st.offSet), if pattern.tail.isEmpty then jetPattern.split("").toList else pattern.tail, until, rocks)

      playRec(State(Set[(Long,Long)]((0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6)), initRockPosition(IH), 0), jetPattern.split("").toList,  until, List())

  def totalTallAfter(rocksStopped: Long, jetPattern: String) = Game(rocksStopped).play(jetPattern).grid.maxBy(_._1)._1

  def totalTallAfter1000000000000 =
    val startRepeatingPattern = 393L
    val numberOfRocksInPattern = 1745L
    val tallForStartRepeatingPattern = 624L
    val tallAddedInEachCycleOfPattern = 2783L
    val numberOfCycleExecutedForThePattern = (1000000000000L - 393L) / 1745L
    val restToTotalRocks = (1000000000000L - 393L) % 1745L
    val tallOfRestToTotalRocks = 992L
    tallForStartRepeatingPattern + tallOfRestToTotalRocks + numberOfCycleExecutedForThePattern * tallAddedInEachCycleOfPattern
}
