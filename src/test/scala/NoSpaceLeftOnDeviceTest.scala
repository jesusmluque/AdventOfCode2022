import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class NoSpaceLeftOnDeviceTest extends AnyFlatSpec {

  "The sum of all directories with at most 100000 for the commands-history1 " should " be 95437 " in {
    assert(NoSpaceLeftOnDevice.totalSumDirFilesWithAtMost100000(Source.fromResource("commands-history1").getLines().toList) == 95437L)
  }

  "The sum of all directories with at most 100000 for the commands-history2 " should " be 1501149 " in {
    assert(NoSpaceLeftOnDevice.totalSumDirFilesWithAtMost100000(Source.fromResource("commands-history2").getLines().toList) == 1501149L)
  }

  "The smallest file to delete to get 300000000 free space in commands-history1 has a size " should " of 10096985 " in {
    assert(NoSpaceLeftOnDevice.sizeOfFileToDeleteToFree300000000(Source.fromResource("commands-history2").getLines().toList) == 10096985L)
  }
}
