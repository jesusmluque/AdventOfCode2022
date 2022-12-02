import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CalorieCountingTest extends AnyFlatSpec {

  "The ELf carrying the most Calories " should "carry 24000 in the test Calories1 " in {
    assert(CalorieCounting.maxCaloriesIn(Source.fromResource("Calories1").getLines().toList) == 24000L)
  }

  "The ELf carrying the most Calories " should "carry 72240 in the test Calories2 " in {
    assert(CalorieCounting.maxCaloriesIn(Source.fromResource("Calories2").getLines().toList) == 72240L)
  }

  "The three ELves carrying the most Calories " should " carry 45000 in the test Calories1 " in {
    assert(CalorieCounting.maxCaloriesInThree(Source.fromResource("Calories1").getLines().toList) == 45000L)
  }

  "The three ELves carrying the most Calories " should " carry 210957 in the test Calories2 " in {
    assert(CalorieCounting.maxCaloriesInThree(Source.fromResource("Calories2").getLines().toList) == 210957L)
  }
}
