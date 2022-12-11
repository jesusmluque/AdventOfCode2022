import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class MonkeyInTheMiddleTest extends AnyFlatSpec {

  "The level of monkey business after 20 rounds for the monkeys from monkeys1 file " should " be 10605 " in {
    assert(MonkeyInTheMiddle.calculateMonkeyBusinessFor20Rounds(Source.fromResource("monkeys1").getLines().toList) == 10605L)
  }

  "The level of monkey business after 20 rounds for the monkeys from monkeys2 file " should " be 117624 " in {
    assert(MonkeyInTheMiddle.calculateMonkeyBusinessFor20Rounds(Source.fromResource("monkeys2").getLines().toList) == 117624L)
  }

  "The level of monkey business after 1000 rounds for the monkeys from monkeys1 file " should " be 2713310158 " in {
    assert(MonkeyInTheMiddle.calculateMonkeyBusinessFor1000Rounds(Source.fromResource("monkeys1").getLines().toList) == 2713310158L)
  }

  "The level of monkey business after 1000 rounds for the monkeys from monkeys2 file " should " be 16792940265 " in {
    assert(MonkeyInTheMiddle.calculateMonkeyBusinessFor1000Rounds(Source.fromResource("monkeys2").getLines().toList) == 16792940265L)
  }
}
