import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TuningTroubleTest extends AnyFlatSpec {

  "The number of characters processed before the first start-of-packet in mjqjpqmgbljsphdztnvjfqwrcgsmlb " should " be 7" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4) == 7)
  }

  "The number of characters processed before the first start-of-packet in mjqjpqmgbljsphdztnvjfqwrcgsmlb " should " be 5" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) == 5)
  }

  "The number of characters processed before the first start-of-packet in nppdvjthqldpwncqszvftbrmjlhg " should " be 6" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("nppdvjthqldpwncqszvftbrmjlhg", 4) == 6)
  }

  "The number of characters processed before the first start-of-packet in nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg " should " be 10" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) == 10)
  }

  "The number of characters processed before the first start-of-packet in zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw " should " be 11" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) == 11)
  }

  "The number of characters processed before the first start-of-packet in mjqjpqmgbljsphdztnvjfqwrcgsmlb when number of start is 14 " should " be 19" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) == 19)
  }

  "The number of characters processed before the first start-of-packet in bvwbjplbgvbhsrlpgdmjqwftvncz when number of start is 14 " should " be 23" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) == 23)
  }

  "The number of characters processed before the first start-of-packet in nppdvjthqldpwncqszvftbrmjlhg when number of start is 14 " should " be 23" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("nppdvjthqldpwncqszvftbrmjlhg", 14) == 23)
  }

  "The number of characters processed before the first start-of-packet in nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg when number of start is 14 " should " be 29" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) == 29)
  }

  "The number of characters processed before the first start-of-packet in zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw  when number of start is 14 " should " be 26" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) == 26)
  }

  "The number of characters processed before the first start-of-packet in file example  when number of start is 14 " should " be 2789" in {
    assert(TuningTrouble.countCharsBeforeStartOfPacket(Source.fromResource("tuning-trouble1").getLines().next(), 14) == 2789)
  }
}
