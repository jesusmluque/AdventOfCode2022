import scala.annotation.tailrec

object RegolithReservoir {

  def totalUnitsBeforeFallIntoTheAbyss(rawRockPositions: List[String]) =
    val rocksPositions = parseRockGrid(rawRockPositions)
    val initSandPos = (0,500)
    sandPath(rocksPositions, initSandPos)

  def totalUnitsBeforeBlockSand(rawRockPositions: List[String]) =
    val rocksPositions = parseRockGrid(rawRockPositions)
    val initSandPos = (0,500)
    sandPath2(rocksPositions, initSandPos)

  private def parseRockGrid(rawRockPositions: List[String]) = {
    val rocksPositions = rawRockPositions.map { path =>
      path.split(" -> ").map(_.split(",")).map(a => (a(1).toInt, a(0).toInt)).toList
    }.foldLeft(Set[(Int, Int)]()) { (setAllPaths, rockPath) =>
      rockPath.foldLeft((List[(Int, Int)](), (0, 0))) { (rockPath, point) =>
        rockPath._2 match {
          case (0, 0) => (point :: rockPath._1, point)
          case p if p._1 == point._1 && p._2 < point._2 => (((p._2 + 1) to point._2).map((p._1, _)).toList ++ rockPath._1, point)
          case p if p._2 == point._2 && p._1 < point._1 => (((p._1 + 1) to point._1).map((_, p._2)).toList ++ rockPath._1, point)
          case p if p._1 == point._1 && p._2 > point._2 => ((point._2 to p._2).map((p._1, _)).toList ++ rockPath._1, point)
          case p if p._2 == point._2 && p._1 > point._1 => ((point._1 to p._1).map((_, p._2)).toList ++ rockPath._1, point)
        }
      }._1.toSet ++ setAllPaths
    }
    rocksPositions
  }

  private def sandPath(rocksPos: Set[(Int,Int)], initSandPos: (Int, Int)):Int =
    val abyssHeight = rocksPos.maxBy(_._1)
    @tailrec
    def sandPathTaiRec(rocks: Set[(Int,Int)], initSand: (Int, Int), acc: Int): Int =
      if initSand._1 == abyssHeight._1 then
        acc
      else if !rocks.contains((initSand._1 + 1, initSand._2)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2), acc)
      else if !rocks.contains((initSand._1 + 1, initSand._2 - 1)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2 - 1), acc)
      else if !rocks.contains((initSand._1 + 1, initSand._2 + 1)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2 + 1), acc)
      else
        sandPathTaiRec(rocks + initSand, (0,500), acc + 1)
    sandPathTaiRec(rocksPos, initSandPos, 0)

  private def sandPath2(rocksPos: Set[(Int,Int)], initSandPos: (Int, Int)):Int =
    val abyssHeight = rocksPos.maxBy(_._1)._1 + 2
    @tailrec
    def sandPathTaiRec(rocks: Set[(Int,Int)], initSand: (Int, Int), acc: Int): Int =
      if initSand._1 == abyssHeight then
        sandPathTaiRec(rocks + initSand, (0,500), acc)
      else if !rocks.contains((initSand._1 + 1, initSand._2)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2), acc)
      else if !rocks.contains((initSand._1 + 1, initSand._2 - 1)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2 - 1), acc)
      else if !rocks.contains((initSand._1 + 1, initSand._2 + 1)) then
        sandPathTaiRec(rocks, (initSand._1 + 1, initSand._2 + 1), acc)
      else if initSand == initSandPos then
        acc + 1
      else
        sandPathTaiRec(rocks + initSand, (0,500), acc + 1)
    sandPathTaiRec(rocksPos, initSandPos, 0)

}
