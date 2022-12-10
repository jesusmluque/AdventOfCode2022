import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class CathodeRayTubeTest extends AnyFlatSpec {

  "The sum of the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles for the cathode-ray-tube1 file " should " be 13140 " in {
    assert(CathodeRayTube.getSumSygnalStrengthAfterInstruction(Source.fromResource("cathode-ray-tube1").getLines().toList) == 13140L)
  }

  "The sum of the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles for the cathode-ray-tube2 file " should " be 14520 " in {
    assert(CathodeRayTube.getSumSygnalStrengthAfterInstruction(Source.fromResource("cathode-ray-tube2").getLines().toList) == 14520L)
  }
}
