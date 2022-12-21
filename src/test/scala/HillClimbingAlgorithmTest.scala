import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HillClimbingAlgorithmTest extends AnyFlatSpec {

  "The fewest steps required to move from your current position to the location that should get the best signal for the grid in hill-grid1 " should " be 31 " in {
    assert(HillClimbingAlgorithm.getFewerStepsPathFor(Source.fromResource("hill-grid1").getLines().toList) == 31L)
  }

  "The fewest steps required to move from your current position to the location that should get the best signal for the grid in hill-grid2 " should " be 330 " in {
    assert(HillClimbingAlgorithm.getFewerStepsPathFor(Source.fromResource("hill-grid2").getLines().toList) == 330L)
  }

  "The fewest steps required to move starting from any square with elevation a to the location with the best signal for the grid in hill-grid1 " should " be 29 " in {
    assert(HillClimbingAlgorithm.getFewerStepsPathForAnyStartingPointAFor(Source.fromResource("hill-grid1").getLines().toList) == 29)
  }

  "The fewest steps required to move starting from any square with elevation a to the location with the best signal for the grid in hill-grid2 " should " be 321 " in {
    assert(HillClimbingAlgorithm.getFewerStepsPathForAnyStartingPointAFor(Source.fromResource("hill-grid2").getLines().toList) == 321)
  }
}
