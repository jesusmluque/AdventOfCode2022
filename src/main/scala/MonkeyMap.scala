import MonkeyMap.Direction.Down

import scala.annotation.tailrec

object MonkeyMap:

  enum Direction(d: String):
    case Right extends Direction("R")
    case Left extends Direction("L")
    case Down extends Direction("D")
    case Up extends Direction("U")
    def value = this match
      case Right => 0
      case Down  => 1
      case Left  => 2
      case Up    => 3

  type Grid = Vector[Vector[String]]
  type Point = ((Int,Int), Direction)

  def getPasswordIn(raw: List[String], isACube: Boolean = false) =
    val (rawGrid, instruction) = raw.splitAt(raw.indexOf(""))
    val gridNonRegular = rawGrid.toVector.map(row => row.split("").toVector)
    val max = gridNonRegular.maxBy(_.length).length
    val grid = gridNonRegular.map(v => if v.length < max then v ++ List.fill(max - v.length)(" ") else v)
    val startingPosition = ((0, grid(0).zipWithIndex.find(a => a._1 == ".").get._2), Direction.Right)
    val res = calculatePath(grid, instruction.last, List(startingPosition), if isACube then moveCube else move)
    (res.head._1._1 + 1) * 1000 + 4 * (res.head._1._2 + 1) + res.head._2.value

  @tailrec
  def calculatePath(g: Grid, instructions: String, path: List[Point], move: (Grid,Point,Int) => Point):List[Point] =
    val (a, rest) = instructions.span(_.isDigit)
    if a != "" then
      val newPos = move(g, path.head, a.toInt)
      calculatePath(g, rest, newPos :: path, move)
    else
      val (a, rest) = instructions.span(_.isUpper)
      if a != "" then
        val newDir = changeDirection(a, path.head)
        calculatePath(g, rest, newDir :: path, move)
      else
        path

  @tailrec
  def move(g: Grid, currentPos: Point, times: Int):Point =
    def findFirstValidPositionForRow(g: Grid, pos: (Int,Int)) =
      val row = pos._1
      val dot = g(row).indexOf(".")
      val sharp = g(row).indexOf("#")
      val col = if dot < sharp || sharp < 0 then dot else pos._2
      (row, col)
    def findFirstValidPositionForCol(g: Grid, pos: (Int,Int)) =
      val col = pos._2
      val dot = g.zipWithIndex.find(v => v._1(col) == ".").map(_._2).get
      val sharp = g.zipWithIndex.find(v => v._1(col) == "#").map(_._2).getOrElse(dot + 1)
      (if dot < sharp || sharp < 0 then dot else pos._1, col)
    def findLastValidPositionForRow(g: Grid, pos: (Int,Int)) =
      val row = pos._1
      val dot = g(row).zipWithIndex.findLast(_._1 == ".").map(_._2).get
      val sharp = g(row).zipWithIndex.findLast(_._1 == "#").map(_._2).getOrElse(dot + 1)
      (row, if dot > sharp || sharp < 0 then dot else pos._2)
    def findLastValidPositionForCol(g: Grid, pos: (Int,Int)) =
      val col = pos._2
      val dot = g.zipWithIndex.findLast(v => v._1(col) == ".").map(_._2).get
      val sharp = g.zipWithIndex.findLast(v => v._1(col) == "#").map(_._2).getOrElse(dot + 1)
      (if dot > sharp || sharp < 0 then dot else pos._1, col)

    if times == 0 || g(currentPos._1._1)(currentPos._1._2) == "#" then
      currentPos
    else
      currentPos match
        case (pos, Direction.Right) if pos._2 + 1 < g(pos._1).length && g(pos._1)(pos._2 + 1) != "#" && g(pos._1)(pos._2 + 1) != " "  => move(g,((pos._1, pos._2 + 1), Direction.Right), times - 1)
        case (pos, Direction.Left) if pos._2 - 1 >= 0 && g(pos._1)(pos._2 - 1) != "#" && g(pos._1)(pos._2 - 1) != " "  => move(g, ((pos._1, pos._2 - 1), Direction.Left), times - 1)
        case (pos, Direction.Down) if pos._1 + 1 < g.length && g(pos._1 + 1)(pos._2) != "#" && g(pos._1 + 1)(pos._2) != " "  => move(g, ((pos._1 + 1, pos._2), Direction.Down), times - 1)
        case (pos, Direction.Up) if pos._1 - 1 >= 0 && g(pos._1 - 1)(pos._2) != "#" && g(pos._1 - 1)(pos._2) != " "  => move(g, ((pos._1 - 1, pos._2), Direction.Up), times - 1)
        case (pos, Direction.Right) if pos._2 + 1 == g(pos._1).length || g(pos._1)(pos._2 + 1) == " " => move(g, (findFirstValidPositionForRow(g, pos), Direction.Right), times -1)
        case (pos, Direction.Left) if pos._2 == 0 || g(pos._1)(pos._2 - 1) == " " => move(g, (findLastValidPositionForRow(g, pos), Direction.Left), times - 1)
        case (pos, Direction.Down) if pos._1 + 1 == g.length || g(pos._1 + 1)(pos._2) == " " => move(g, (findFirstValidPositionForCol(g, pos), Direction.Down), times - 1)
        case (pos, Direction.Up) if pos._1 == 0 || g(pos._1 - 1)(pos._2) == " " => move(g, (findLastValidPositionForCol(g, pos), Direction.Up), times - 1)
        case p => p

  @tailrec
  def moveCube(g: Grid, currentPos: Point, times: Int):Point =
    if times == 0 || g(currentPos._1._1)(currentPos._1._2) == "#" then
      currentPos
    else
      currentPos match
        case (pos, Direction.Right) if pos._2 + 1 < g(pos._1).length && g(pos._1)(pos._2 + 1) != "#" && g(pos._1)(pos._2 + 1) != " "  => moveCube(g,((pos._1, pos._2 + 1), Direction.Right), times - 1)
        case (pos, Direction.Left) if pos._2 - 1 >= 0 && g(pos._1)(pos._2 - 1) != "#" && g(pos._1)(pos._2 - 1) != " "  => moveCube(g, ((pos._1, pos._2 - 1), Direction.Left), times - 1)
        case (pos, Direction.Down) if pos._1 + 1 < g.length && g(pos._1 + 1)(pos._2) != "#" && g(pos._1 + 1)(pos._2) != " "  => moveCube(g, ((pos._1 + 1, pos._2), Direction.Down), times - 1)
        case (pos, Direction.Up) if pos._1 - 1 >= 0 && g(pos._1 - 1)(pos._2) != "#" && g(pos._1 - 1)(pos._2) != " "  => moveCube(g, ((pos._1 - 1, pos._2), Direction.Up), times - 1)
        case p@(pos, Direction.Right) if pos._2 + 1 == g(pos._1).length || g(pos._1)(pos._2 + 1) == " " => moveCube(g, findNextInCube(g, p), times -1)
        case p@(pos, Direction.Left) if pos._2 == 0 || g(pos._1)(pos._2 - 1) == " " => moveCube(g, findNextInCube(g, p), times - 1)
        case p@(pos, Direction.Down) if pos._1 + 1 == g.length || g(pos._1 + 1)(pos._2) == " " => moveCube(g, findNextInCube(g, p), times - 1)
        case p@(pos, Direction.Up) if pos._1 == 0 || g(pos._1 - 1)(pos._2) == " " => moveCube(g, findNextInCube(g, p), times - 1)
        case p => p

  def findNextInCube(g: Grid, pos: Point) =
    val newPos = pos match
      //1
      case ((a, 50), Direction.Left) if a >= 0 && a < 50 => ((149 - a, 0), Direction.Right)
      case ((0, b), Direction.Up) if b >= 50 && b < 100 => ((100 + b, 0), Direction.Right)
      //2
      case ((0, b), Direction.Up) if b >= 100 && b < 150 => ((199, b - 100), Direction.Up)
      case ((a,149), Direction.Right) if a >= 0 && a < 50 => ((149 - a, 99), Direction.Left)
      case ((49, b), Direction.Down) if b >= 100 && b < 150 => ((b - 100 + 50, 99), Direction.Left)
      //3
      case ((a, 50), Direction.Left) if a >= 50 && a < 100 => ((100, a - 50), Direction.Down)
      case ((a, 99), Direction.Right) if a >= 50 && a < 100 => ((49, a - 50 + 100), Direction.Up)
      //4
      case ((100, b), Direction.Up) if b >= 0 && b < 50 => ((50 + b, 50), Direction.Right)
      case ((a, 0), Direction.Left) if a >= 100 && a < 150 => ((49 - a + 100, 50), Direction.Right)
      //5
      case ((a, 99), Direction.Right) if a >= 100 && a < 150 => ((49 - a + 100, 149), Direction.Left)
      case ((149, b), Direction.Down) if b >= 50 && b < 100 => ((b - 50 + 150, 49), Direction.Left)
      //6
      case ((a, 0), Direction.Left) if a >= 150 && a < 200 => ((0, a - 150 + 50), Direction.Down)
      case ((199, b), Direction.Down) if b >= 0 && b < 50 => ((0, b + 100), Direction.Down)
      case ((a, 49), Direction.Right) if a >= 150 && a < 200 => ((149, a - 150 + 50), Direction.Up)

    if g(newPos._1._1)(newPos._1._2) == "#" then
      pos
    else
      newPos

  def changeDirection(instruction: String, currentPos: Point) =
    instruction match
      case "L" if currentPos._2 == Direction.Up => (currentPos._1, Direction.Left)
      case "L" if currentPos._2 == Direction.Right => (currentPos._1, Direction.Up)
      case "L" if currentPos._2 == Direction.Down => (currentPos._1, Direction.Right)
      case "L" if currentPos._2 == Direction.Left => (currentPos._1, Direction.Down)
      case "R" if currentPos._2 == Direction.Up => (currentPos._1, Direction.Right)
      case "R" if currentPos._2 == Direction.Right => (currentPos._1, Direction.Down)
      case "R" if currentPos._2 == Direction.Down => (currentPos._1, Direction.Left)
      case "R" if currentPos._2 == Direction.Left => (currentPos._1, Direction.Up)