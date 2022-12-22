import scala.annotation.tailrec
import scala.collection.mutable

object HillClimbingAlgorithm:

  case class Grid(g: Vector[Vector[Int]]):
    def surroundingPositions(pos: (Int, Int)) = List((1,0), (0,1), (-1,0), (0,-1)).foldLeft(List[(Int, Int)]()) {(acc, n) =>
      val newPos = (n._1 + pos._1, n._2 + pos._2)

      if newPos._1 >= 0 && newPos._1 < this.g.length && newPos._2 >= 0 && newPos._2 < this.g.head.length then
        val nextValue = if g(newPos._1)(newPos._2) == 69 then 122 else g(newPos._1)(newPos._2)
        val oldValue = if g(pos._1)(pos._2) == 83 then 97 else g(pos._1)(pos._2)
        if nextValue <= oldValue + 1 then
          newPos :: acc
        else
          acc
      else acc
    }
    def getPositionForAll(elevation: String) =
      val el = elevation.toCharArray
      this.g.zipWithIndex.filter(n => n._1.contains(el(0).toInt)).flatMap(n => n._1.zipWithIndex.filter(_._1 == el(0).toInt).map(p => (n._2,p._2)))

    def getPositionFor(elevation: String) =
      val el = elevation.toCharArray
      this.g.zipWithIndex.map(n => (n._2, n._1.indexOf(el(0).toInt))).find(_._2 != -1)

  object Grid:
    def apply(raw: List[String]):Grid = Grid(raw.foldLeft(Vector[Vector[Int]]()) { (acc, row) =>
      acc :+ row.toCharArray.map(_.toInt).toVector
    })

  def getFewerStepsPathFor(rawGrid: List[String]) =
    val grid = Grid(rawGrid)
    val startPosition = grid.getPositionFor("S")
    val endPosition = grid.getPositionFor("E")
    val queue = mutable.Queue[List[(Int, Int)]](List(startPosition.getOrElse(0,0)))
    val len = findEndFrom(endPosition.getOrElse(0,0), grid, queue, Set())
    len.length - 1

  def getFewerStepsPathForAnyStartingPointAFor(rawGrid: List[String]) =
    val grid = Grid(rawGrid)
    val aStartingPoints = grid.getPositionForAll("a") ++ Vector((0,0))
    val queue = mutable.Queue[List[(Int, Int)]]()
    val endPosition = grid.getPositionFor("E")
    val res = aStartingPoints.map{p =>
      queue.clear()
      findEndFrom(endPosition.get, grid, queue.addOne(List(p)), Set()).length - 1}.filter(_ != -1)
    res.min

  @tailrec
  private def findEndFrom(endPosition: (Int, Int), grid: Grid, queue: mutable.Queue[List[(Int,Int)]], visited: Set[(Int,Int)]): List[(Int,Int)] =
    if queue.isEmpty then
      List()
    else
      val path = queue.dequeue()
      val point = path.head
      if point == endPosition then
        path
      else if !visited.contains(point) then
        val nextCandidates = grid
          .surroundingPositions(point)
          .filterNot(visited)
          .map { pt =>
            pt +: path
          }
        queue.enqueueAll(nextCandidates)
        findEndFrom(endPosition, grid, queue, visited + point)
      else
        findEndFrom(endPosition, grid, queue, visited)