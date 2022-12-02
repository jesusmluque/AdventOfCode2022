import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class RockPaperScissorsTest extends AnyFlatSpec {

  "The total scores for the rock-paper-scissors-strategy1 test case using first strategy " should " be 15 " in {
    assert(RockPaperScissors.getScore(Source.fromResource("rock-paper-scissors-strategy1").getLines().toList) == 15)
  }

  "The total scores for the rock-paper-scissors-strategy2 test case using first strategy " should " be 8392 " in {
    assert(RockPaperScissors.getScore(Source.fromResource("rock-paper-scissors-strategy2").getLines().toList) == 8392)
  }

  "The total scores for the rock-paper-scissors-strategy1 test case using second strategy " should " be 12 " in {
    assert(RockPaperScissors.getScoreSecondStrategy(Source.fromResource("rock-paper-scissors-strategy1").getLines().toList) == 12)
  }

  "The total scores for the rock-paper-scissors-strategy2 test case using second strategy " should " be 10116 " in {
    assert(RockPaperScissors.getScoreSecondStrategy(Source.fromResource("rock-paper-scissors-strategy2").getLines().toList) == 10116)
  }


}
