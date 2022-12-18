import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class RegolithReservoirTest extends AnyFlatSpec {
  "The total units of sands before starting to fall in the the abyss for the cave-grid1 file " should " be 24 " in {
    assert(RegolithReservoir.totalUnitsBeforeFallIntoTheAbyss(Source.fromResource("cave-grid1").getLines().toList) == 24)
  }

  "The total units of sands before starting to fall in the the abyss for the cave-grid2 file " should " be 1199 " in {
    assert(RegolithReservoir.totalUnitsBeforeFallIntoTheAbyss(Source.fromResource("cave-grid2").getLines().toList) == 1199)
  }

  "The total units of sands needed to block the sand on starting point for the cave-grid1 file " should " be 93 " in {
    assert(RegolithReservoir.totalUnitsBeforeBlockSand(Source.fromResource("cave-grid1").getLines().toList) == 93)
  }

  "The total units of sands needed to block the sand on starting point for the cave-grid2 file " should " be 23925 " in {
    assert(RegolithReservoir.totalUnitsBeforeBlockSand(Source.fromResource("cave-grid2").getLines().toList) == 23925)
  }
}
