import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BoilingBouldersTest extends AnyFlatSpec {

  "The surface area for the scanned lava droplet for the boiling-boulders1 " should " be 64 " in {
    assert(BoilingBoulders.getSurfaceFor(Source.fromResource("boiling-boulders1").getLines().toList) == 64L)
  }

  "The surface area for the scanned lava droplet for the boiling-boulders2 " should " be 4460 " in {
    assert(BoilingBoulders.getSurfaceFor(Source.fromResource("boiling-boulders2").getLines().toList) == 4460L)
  }

  "The surface area for the scanned lava droplet taking into account the trapped air cubes for the boiling-boulders1 " should " be 64 " in {
    assert(BoilingBoulders.getSurfaceIncludingTrappedAirCubesFor(Source.fromResource("boiling-boulders1").getLines().toList) == 58L)
  }

  "The surface area for the scanned lava droplet taking into account the trapped air cubes for the boiling-boulders2 " should " be 64 " in {
    assert(BoilingBoulders.getSurfaceIncludingTrappedAirCubesFor(Source.fromResource("boiling-boulders2").getLines().toList) == 64L)
  }
}
