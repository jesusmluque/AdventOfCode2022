import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class PyroclasticFlowTest extends AnyFlatSpec {

  "The total units ot total tall after 2022 rocks have stopped taking into account the jet pattern in file jet-pattern1 " should " be 3068 " in {
    assert(PyroclasticFlow.totalTallAfter(2022, Source.fromResource("jet-pattern1").getLines.next()) == 3068)
  }

  "The total units ot total tall after 2022 rocks have stopped taking into account the jet pattern in file jet-pattern2 " should " be 3239 " in {
    assert(PyroclasticFlow.totalTallAfter(2022, Source.fromResource("jet-pattern2").getLines.next()) == 3239)
  }

  "The total units ot total tall after 2022 rocks have stopped taking into account the jet pattern in file jet-pattern2 " should " be 1594842406882 " in {
    assert(PyroclasticFlow.totalTallAfter1000000000000 == 1594842406882L)
  }
}
