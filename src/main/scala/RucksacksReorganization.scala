object RucksacksReorganization {

  private val az = buildMapToCalculatePriorityFor('a' to 'z', 1L)
  private val AZ = buildMapToCalculatePriorityFor('A' to 'Z', 27L)

  def totalPriority(rucksacks: List[String]):Long =
    rucksacks.foldLeft(0L) { (acc, rucksack) =>
      val (compartment1, compartment2) = rucksack.splitAt(rucksack.length/2)
      val items = compartment1.toSet.intersect(compartment2.toSet)

      items.flatMap(i => calculatePriorityForChar(i)).map(_ + acc).headOption.getOrElse(acc)
    }

  def totalPriorityByGroupOfThree(rucksacks: List[String]) =
    rucksacks.grouped(3).foldLeft(0L) { (acc, group) =>
      group.map(_.toSet).reduce { (s, c) =>
        s intersect c
      }.flatMap(calculatePriorityForChar(_).map(_ + acc)).headOption.getOrElse(acc)
    }

  private def calculatePriorityForChar(i: Char): Option[Long] =
    az.get(i) orElse AZ.get(i)

  private def buildMapToCalculatePriorityFor(range: Seq[Char], offSet: Long) =
    range.zipWithIndex.foldLeft(Map[Char, Long]())((acc, x) => acc + (x._1 -> x._2.toLong)).map(m => (m._1, m._2 + offSet))

}
