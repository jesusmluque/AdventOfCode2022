object BeaconExclusionZone:

  case class Instruction(sensor: (Int, Int), beacon: (Int,Int)):
    val distance:Int = BeaconExclusionZone.distance(sensor,beacon)

  object Instruction:
    def apply(raw: String):Instruction =
      val pattern = "Sensor at x=([-]*[0-9]+), y=([-]*[0-9]+): closest beacon is at x=([-]*[0-9]+), y=([-]*[0-9]+)".r
      val pattern(x,y,z,q) = raw
      Instruction((x.toInt,y.toInt), (z.toInt,q.toInt))

  def calculateNoBeaconsPositionsFor(targetRow: Int, rawInstructions: List[String]) =
    val instructions = rawInstructions.map(Instruction(_))
    val min = instructions.minBy(s => s.sensor._1 - s.distance)
    val max = instructions.maxBy(s => s.sensor._1 + s.distance)
    val instructionFiltered = instructions.filter(i => distance((i.sensor._1, targetRow),i.sensor) < i.distance)
    ((min.sensor._1 - min.distance) to (max.sensor._1 + max.distance)).foldLeft(0) { (acc, r) =>
      val p = (r, targetRow)
      if instructionFiltered.map(i => distance(p, i.sensor) <= i.distance && i.beacon != p).count(a => a) > 0 then
        1 + acc
      else
        acc
    }

  def calculateBeaconPositionTunningFrequencyIn(rawInstructions: List[String], rowMin: Int, rowMax: Int) =
    val instructions = rawInstructions.map(Instruction(_))
    val point = (rowMin to rowMax).map { row =>
      val instructionFiltered = instructions.filter(i => distance((i.sensor._1, row),i.sensor) < i.distance)
      val ranges = instructionFiltered.foldLeft(List[(Int,Int)]()) { (acc, i) =>
        (-i.distance + (i.sensor._2 - row).abs + i.sensor._1, i.distance - (i.sensor._2 - row).abs + i.sensor._1) :: acc
      }.sortBy(_._1)
      ranges.foldLeft((false, (0,row), 0)){ (acc, r) =>
        val (found,target,last) = acc
        if found then
          acc
        else if r._1 <= last + 1 then
          (false, target, if r._2 > last then r._2 else last)
        else
          (true, (last + 1, target._2), last)
      }
    }.filter(_._1)(0)
    point._2._1 * 4000000L + point._2._2

  def distance(p1:(Int,Int),p2:(Int,Int)) =
    val distanceY = p1._1 - p2._1
    val distanceX = p1._2 - p2._2
    (if distanceY < 0 then -1*distanceY else distanceY) + (if distanceX < 0 then -1*distanceX else distanceX)
