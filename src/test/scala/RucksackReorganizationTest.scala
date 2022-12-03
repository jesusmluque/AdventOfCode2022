import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class RucksackReorganizationTest extends AnyFlatSpec {

  "The sum of priorites for the item that are in both compartments for the rucksacks1 file " should " be 157 " in {
    assert(RucksacksReorganization.totalPriority(Source.fromResource("rucksacks1").getLines().toList) == 157)
  }

  "The sum of priorites for the item that are in both compartments for the rucksacks2 file " should " be 7826 " in {
    assert(RucksacksReorganization.totalPriority(Source.fromResource("rucksacks2").getLines().toList) == 7826)
  }

  "The sum of priorites for the item that are in both compartments for the rucksacks3 file " should " be 1865 " in {
    assert(RucksacksReorganization.totalPriority(Source.fromResource("rucksacks3").getLines().toList) == 1865)
  }

  "The sum of priorities for each three-Elf group for the rucksacks1 file " should " be 70 " in {
    assert(RucksacksReorganization.totalPriorityByGroupOfThree(Source.fromResource("rucksacks1").getLines().toList) == 70)
  }

  "The sum of priorities for each three-Elf group for the rucksacks2 file " should " be 2577 " in {
    assert(RucksacksReorganization.totalPriorityByGroupOfThree(Source.fromResource("rucksacks2").getLines().toList) == 2577)
  }
}
