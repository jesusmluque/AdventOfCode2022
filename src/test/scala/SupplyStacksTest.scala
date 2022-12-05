import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SupplyStacksTest extends AnyFlatSpec {

  "The list of crates at the end of the rearrangement for the instructions in supply-stacks1 " should " be CMZ " in {
    assert(SupplyStacks.getCratesFromTopOfStacksAfterInstructionApplied(Source.fromResource("supply-stacks1").getLines().toList) == "CMZ")
  }

  "The list of crates at the end of the rearrangement for the instructions in supply-stacks2 " should " be FWSHSPJWM " in {
    assert(SupplyStacks.getCratesFromTopOfStacksAfterInstructionApplied(Source.fromResource("supply-stacks2").getLines().toList) == "FWSHSPJWM")
  }

  "The list of crates at the end of the rearrangement for the instructions in supply-stacks3 " should " be CBGSMSWTS " in {
    assert(SupplyStacks.getCratesFromTopOfStacksAfterInstructionApplied(Source.fromResource("supply-stacks3").getLines().toList) == "CBGSMSWTS")
  }

  "The list of crates at the end of the rearrangement for the instructions in supply-stacks1 with the new 9001 " should " be MCD " in {
    assert(SupplyStacks.getCratesFromTopOfStacksAfterInstructionApplied(Source.fromResource("supply-stacks1").getLines().toList, "9001") == "MCD")
  }

  "The list of crates at the end of the rearrangement for the instructions in supply-stacks2 with the new 9001  " should " be PWPWHGFZS " in {
    assert(SupplyStacks.getCratesFromTopOfStacksAfterInstructionApplied(Source.fromResource("supply-stacks2").getLines().toList, "9001") == "PWPWHGFZS")
  }
}
