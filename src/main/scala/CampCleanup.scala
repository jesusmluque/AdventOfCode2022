object CampCleanup {

  case class Range(private val init:Int, private val end:Int)

  object Range {
    def apply(rawRange: String): Range = {
      val Array(i, e) = rawRange.split("-").map(_.toInt)
      Range(i, e)
    }
    def isFullyOverlap(r1: Range, r2: Range) =
      if (r1.init <= r2.init && r1.end >= r2.end)
        true
      else if (r1.init >= r2.init && r1.end <= r2.end)
        true
      else
        false
    def isOverlap(r1: Range, r2: Range) =
      if (r1.init >= r2.init && r1.init <= r2.end)
        true
      else if (r2.init >= r1.init && r2.init <= r1.end)
        true
      else
        false
  }

  def calculatePairFullyOverlaps(pairRangesRaw: List[String]) =
    countByConditionFor(pairRangesRaw, Range.isFullyOverlap)

  def calculatePairOverlaps(pairRangesRaw: List[String]) =
    countByConditionFor(pairRangesRaw, Range.isOverlap)

  def countByConditionFor(pairRangesRaw: List[String], condition: (Range, Range) => Boolean) =
    pairRangesRaw.map { raw =>
      raw.split(",").map(Range(_))
    }.count(pair => condition(pair(0), pair(1)))

}
