import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UnstableDiffusion:

  enum Direction:
    case North
    case South
    case West
    case East

  case class Directions(queue: Queue[Direction]):
    def pop =
      val (value, rest) = queue.dequeue
      (value, Directions(rest.enqueue(value)))
  object Directions:
    def apply():Directions = Directions(Queue(Direction.North, Direction.South, Direction.West, Direction.East))

  type Point = (Int,Int)
  case class Grid(private val grid: Set[Point], private val oldValues: Map[Point, Point] = Map()):
    lazy val maxSize = (grid.maxBy(_._1)._1 - grid.minBy(_._1)._1 + 1) * (grid.maxBy(_._2)._2 - grid.minBy(_._2)._2 + 1)
    def countEmptyPositions =
      maxSize - grid.size
    def contain(p: Point) = grid.contains(p)
    def remove(p: Point) = Grid(grid - p, oldValues)
    def isPointAlone(p: Point) =
      val points = List((p._1 - 1, p._2 - 1), (p._1 - 1, p._2), (p._1 - 1, p._2 + 1), (p._1, p._2 - 1), (p._1, p._2 + 1), (p._1 + 1, p._2 - 1), (p._1 + 1, p._2), (p._1 + 1, p._2 + 1))
      points.forall(a =>
        !grid.contains(a))
    def foldLeft(newG: Grid)(f: (Grid, Point) => Grid) =
      val res = grid.foldLeft(newG)((a,b) => f(a, b))
      Grid(res.grid, res.oldValues)
    def isOccupiedFor(direction: Direction, elf: Point) = direction match
      case Direction.North if List((elf._1 - 1, elf._2),(elf._1 - 1, elf._2 - 1),(elf._1 - 1, elf._2 + 1)).forall(!this.contain(_)) => false
      case Direction.South if List((elf._1 + 1, elf._2),(elf._1 + 1, elf._2 - 1),(elf._1 + 1, elf._2 + 1)).forall(!this.contain(_)) => false
      case Direction.West if List((elf._1, elf._2 - 1),(elf._1 - 1, elf._2 - 1),(elf._1 + 1, elf._2 - 1)).forall(!this.contain(_)) => false
      case Direction.East if List((elf._1, elf._2 + 1),(elf._1 - 1, elf._2 + 1),(elf._1 + 1, elf._2 + 1)).forall(!this.contain(_)) => false
      case _ => true
  object Grid:
    def apply(rawPositions: List[String]):Grid =
      rawPositions.zipWithIndex.foldLeft(Grid(Set[Point]())) { (set, row) =>
        row._1.split("").zipWithIndex.filter(_._1 == "#").foldLeft(set){ (a, p) =>
          Grid(a.grid + ((row._2, p._2)))
        }
      }
    def apply():Grid = Grid(Set[Point](), Map())
    def addFromOneToOther(g: Grid, newG: Grid, p: Point, directions: Directions) =
      val dir = directions.pop
      if g.isPointAlone(p) then
        Grid(newG.grid + p, newG.oldValues)
      else
        val optionDir = if !g.isOccupiedFor(dir._1, p) then Some(dir._1) else
          val dir2 = dir._2.pop
          if !g.isOccupiedFor(dir2._1, p) then Some(dir2._1) else
            val dir3 = dir2._2.pop
            if !g.isOccupiedFor(dir3._1, p) then Some(dir3._1) else
              val dir4 = dir3._2.pop
              if !g.isOccupiedFor(dir4._1, p) then Some(dir4._1) else None
        val newPoint = optionDir match
          case Some(Direction.North) => (p._1 - 1, p._2)
          case Some(Direction.South) => (p._1 + 1, p._2)
          case Some(Direction.West) => (p._1, p._2 - 1)
          case Some(Direction.East) => (p._1, p._2 + 1)
          case None => p
        if newG.contain(newPoint) then
          val oldPoint = newG.oldValues.get(newPoint)
          Grid(newG.remove(newPoint).grid + (oldPoint.get, p), newG.oldValues)
        else
          Grid(newG.grid + newPoint, newG.oldValues.updated(newPoint, p))

  def calculateEmptyTilesAfter(rounds: Int, rawPositions: List[String]) =
    @tailrec
    def findFinalPositions(r: Int, g: Grid, directions: Directions):Grid =
      if r == 0 then g
      else
        val newGrid = g.foldLeft(Grid(Set())) { (newG, elf) =>
          Grid.addFromOneToOther(g, newG, elf, directions)
        }
        findFinalPositions(r - 1, newGrid, directions.pop._2)

    val grid = Grid(rawPositions)
    findFinalPositions(rounds, grid, Directions()).countEmptyPositions

  def getLastRound(rawPositions: List[String]) =
    def findFinalPositions(g: Grid, directions: Directions, round: Int):Int =
      val newGrid = g.foldLeft(Grid(Set())) { (newG, elf) =>
        Grid.addFromOneToOther(g, newG, elf, directions)
      }
      if newGrid == g then round else findFinalPositions(newGrid, directions.pop._2, round + 1)

    val grid = Grid(rawPositions)
    findFinalPositions(grid, Directions(), 0)