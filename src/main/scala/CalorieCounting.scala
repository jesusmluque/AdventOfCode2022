object CalorieCounting {

  def maxCaloriesIn(elves: List[String]):Long =
    getElvesCaloriesList(elves).max

  def maxCaloriesInThree(elves: List[String]): Long =
    getElvesCaloriesList(elves).sorted(Ordering.Long.reverse).take(3).sum

  private def getElvesCaloriesList(elves: List[String]) =
    elves.foldLeft(List[Long]()) { (acc, next) =>
      next match
        case "" => 0 :: acc
        case nst if acc.nonEmpty => (nst.toLong + acc.head) :: acc.tail
        case nst => nst.toLong :: acc
      }
}
