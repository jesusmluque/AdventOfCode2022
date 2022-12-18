import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class DistressSignalTest extends AnyFlatSpec {

  "The sum of all indices of the pairs in right order for the input packets1 file " should " be 13 " in {
    assert(DistressSignal.rightOrderIndicesSumFor(Source.fromResource("packets1").getLines().toList) == 13)
  }

  "The sum of all indices of the pairs in right order for the input packets3 file " should " be 1 " in {
    assert(DistressSignal.rightOrderIndicesSumFor(Source.fromResource("packets3").getLines().toList) == 1)
  }

  "The sum of all indices of the pairs in right order for the input packets2 file " should " be 6656 " in {
    assert(DistressSignal.rightOrderIndicesSumFor(Source.fromResource("packets2").getLines().toList) == 6656)
  }

  "After order all packet with the divider included, the multiplication of the position of each divider for packets1 " should " is 140 " in {
    assert(DistressSignal.calculateDecoderKey(Source.fromResource("packets1").getLines().toList) == 140)
  }

  "After order all packet with the divider included, the multiplication of the position of each divider for packets2 " should " is 19716 " in {
    assert(DistressSignal.calculateDecoderKey(Source.fromResource("packets2").getLines().toList) == 19716)
  }
}
