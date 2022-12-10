import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class RopeBridgeTest extends AnyFlatSpec {

  "The number of positions the tail visited at least one for the movements of head in rope-bridge1 file " should " be 13 " in {
    assert(RopeBridge.countPointsVisitedByTailIn(Source.fromResource("rope-bridge1").getLines().toList) == 13)
  }

  "The number of positions the tail visited at least one for the movements of head in rope-bridge2 file " should " be 6391 " in {
    assert(RopeBridge.countPointsVisitedByTailIn(Source.fromResource("rope-bridge2").getLines().toList) == 6391)
  }

  "The number of positions the long version tail visited at least one for the movements of head in rope-bridge1 file " should " be 36 " in {
    assert(RopeBridge.countPointsVisitedByLongTailIn(Source.fromResource("rope-bridge3").getLines().toList) == 36)
  }

  "The number of positions the long version tail visited at least one for the movements of head in rope-bridge2 file " should " be 2593 " in {
    assert(RopeBridge.countPointsVisitedByLongTailIn(Source.fromResource("rope-bridge2").getLines().toList) == 2593)
  }
}
