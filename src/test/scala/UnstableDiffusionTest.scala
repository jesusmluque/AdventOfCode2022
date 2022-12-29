import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class UnstableDiffusionTest extends AnyFlatSpec {

  "The number of empty tiles in the smallest rectangle after 10 rounds of Elves process separation from init position in elves-positions1 file " should " be 110 " in {
    assert(UnstableDiffusion.calculateEmptyTilesAfter(10, Source.fromResource("elves-positions1").getLines().toList) == 110)
  }

  "The number of empty tiles in the smallest rectangle after 10 rounds of Elves process separation from init position in elves-positions2 file " should " be 4181 " in {
    assert(UnstableDiffusion.calculateEmptyTilesAfter(10, Source.fromResource("elves-positions2").getLines().toList) == 4181)
  }

  "The round where all the Elves stop moving for the elves-position1 file " should " be 20 " in {
    assert(UnstableDiffusion.getLastRound(Source.fromResource("elves-positions1").getLines().toList) == 20)
  }

  "The round where all the Elves stop moving for the elves-position2 file " should " be 973 " in {
    assert(UnstableDiffusion.getLastRound(Source.fromResource("elves-positions2").getLines().toList) == 973)
  }
}
