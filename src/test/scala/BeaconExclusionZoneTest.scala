import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class BeaconExclusionZoneTest extends AnyFlatSpec {

  "The number of positions where no beacons can be for the row 10 in the sensor-beacons1 file " should " be 26 " in {
    assert(BeaconExclusionZone.calculateNoBeaconsPositionsFor(10, Source.fromResource("sensor-beacons1").getLines().toList) == 26)
  }

  "The number of positions where no beacons can be for the row 10 in the sensor-beacons2 file " should " be 4717631 " in {
    assert(BeaconExclusionZone.calculateNoBeaconsPositionsFor(2000000, Source.fromResource("sensor-beacons2").getLines().toList) == 4717631)
  }

  "The tuning frequency for the only position of the distress signal in the sensor-beacon1 file " should " be 56000011 " in {
    assert(BeaconExclusionZone.calculateBeaconPositionTunningFrequencyIn(Source.fromResource("sensor-beacons1").getLines().toList, 0, 20) == 56000011L)
  }

  "The tuning frequency for the only position of the distress signal in the sensor-beacon2 file " should " be 13197439355220 " in {
    assert(BeaconExclusionZone.calculateBeaconPositionTunningFrequencyIn(Source.fromResource("sensor-beacons2").getLines().toList, 0, 4000000) == 13197439355220L)
  }
}
