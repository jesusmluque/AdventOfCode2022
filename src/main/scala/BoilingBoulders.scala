import scala.annotation.tailrec

object BoilingBoulders:

  def distance(p1: (Int,Int,Int), p2: (Int,Int,Int)) =
    (p1._1 - p2._1).abs + (p1._2 - p2._2).abs + (p1._3 - p2._3).abs

  def getSurfaceFor(rawDropletGrid: List[String]) =
    val grid = parseGrid(rawDropletGrid)
    getSurfaceWithoutTrappedAirCubes(grid)

  def getSurfaceIncludingTrappedAirCubesFor(rawDropletGrid: List[String]) =
    val grid = parseGrid(rawDropletGrid)
    val range = getMinMaxRange(grid)
    val min = range._1
    val surfacePoints = floodPoints(grid, List(), List((min, min, min)), Set())
    surfacePoints.toList.sortBy(_._1).flatMap{ s =>
      println(s"${s} -> ${getNeighbors(s, min, range._2)} -> ${getNeighbors(s, min, range._2).filter(n => !grid.contains(n))}")
      getNeighbors(s, min, range._2).filter(n => !grid.contains(n))
    }.size

  private def getMinMaxRange(grid: List[(Int, Int, Int)]) = {
    val min = List(grid.minBy(_._1)._1, grid.minBy(_._2)._2, grid.minBy(_._3)._3).min
    val max = List(grid.maxBy(_._1)._1, grid.maxBy(_._2)._2, grid.maxBy(_._3)._3).max
    (min, max)
  }

  @tailrec
  private def floodPoints(grid: List[(Int,Int,Int)], visited: List[(Int,Int,Int)], floodedPoints: List[(Int,Int,Int)], surfacePoints: Set[(Int,Int,Int)]):Set[(Int,Int,Int)] = {
    val range = getMinMaxRange(grid)
    if floodedPoints.isEmpty then
      surfacePoints
    else
      val currentPoint = floodedPoints.head
      if !visited.contains(currentPoint) then
        val neighbors = getNeighbors(currentPoint, range._1, range._2)
        floodPoints(grid, currentPoint :: visited, neighbors.foldLeft(floodedPoints.tail)((acc, n) => if grid.contains(n) then acc else n :: acc), neighbors.foldLeft(surfacePoints)((acc,n) => if grid.contains(n) then acc + n else acc))
      else
        floodPoints(grid, visited, floodedPoints.tail, surfacePoints)
  }

  private def getNeighbors(point: (Int, Int, Int), min: Int, max: Int) =
    List((point._1 + 1, point._2, point._3), (point._1, point._2 + 1, point._3), (point._1, point._2, point._3 + 1),
      (point._1 - 1, point._2, point._3), (point._1, point._2 - 1, point._3), (point._1, point._2, point._3 - 1))
      .filter(c => c._1 >= min && c._1 <= max && c._2 >= min && c._2 <= max && c._3 >= min && c._3 <= max)

  private def getSurfaceWithoutTrappedAirCubes(grid: List[(Int, Int, Int)]) = {
    val connectedCount = grid.foldLeft((0L, grid.tail)) { (acc, p) =>
      if acc._2.isEmpty then
        acc
      else
        val connected = acc._2.count(a => distance(a, p) == 1)
        (connected * 2L + acc._1, acc._2.tail)
    }
    (grid.size.toLong * 6L) - connectedCount._1
  }

  private def parseGrid(rawDropletGrid: List[String]) = {
    val grid = rawDropletGrid.map { p =>
      val vector = p.split(",")
      (vector(0).toInt, vector(1).toInt, vector(2).toInt)
    }
    grid
  }

  private def combinations(l: List[Int]) =
    l.flatMap { x =>
      l.flatMap{ y =>
        l.map{ z =>
          (x,y,z)
        }
      }
    }