import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CampCleanupTest extends AnyFlatSpec {

  "The number of fully overlaps between pair of ranges for the camp-pairs1 file " should " be 2 " in {
    assert(CampCleanup.calculatePairFullyOverlaps(Source.fromResource("camp-pairs1").getLines().toList) == 2)
  }

  "The number of fully overlaps between pair of ranges for the camp-pairs2 file " should " be 448 " in {
    assert(CampCleanup.calculatePairFullyOverlaps(Source.fromResource("camp-pairs2").getLines().toList) == 448)
  }

  "The number of overlaps between pair of ranges for the camp-pairs1 file " should " be 4 " in {
    assert(CampCleanup.calculatePairOverlaps(Source.fromResource("camp-pairs1").getLines().toList) == 4)
  }

  "The number of overlaps between pair of ranges for the camp-pairs2 file " should " be 794 " in {
    assert(CampCleanup.calculatePairOverlaps(Source.fromResource("camp-pairs2").getLines().toList) == 794)
  }
}
