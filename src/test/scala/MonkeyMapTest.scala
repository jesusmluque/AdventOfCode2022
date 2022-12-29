import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class MonkeyMapTest extends AnyFlatSpec {

  "The final password for the final position in the monkey-map1 file " should " be 6032 " in {
    assert(MonkeyMap.getPasswordIn(Source.fromResource("monkey-map1").getLines().toList) == 6032)
  }

  "The final password for the final position in the monkey-map2 file " should " be 1428 " in {
    assert(MonkeyMap.getPasswordIn(Source.fromResource("monkey-map2").getLines().toList) == 1428)
  }

  "The final password for the final position for a cube in the monkey-map2 file " should " be 142380 " in {
    assert(MonkeyMap.getPasswordIn(Source.fromResource("monkey-map2").getLines().toList, true) == 142380)
  }
}
