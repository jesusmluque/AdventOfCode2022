import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TreeTopTreeHouseTest extends AnyFlatSpec {

  "The number of trees visible out side the greed of the tree-greed1 file " should " be 21 " in {
    assert(TreeTopTreeHouse.countVisibleTreesIn(Source.fromResource("tree-greed1").getLines().toVector) == 21)
  }

  "The number of trees visible out side the greed of the tree-greed2 file " should " be 1779 " in {
    assert(TreeTopTreeHouse.countVisibleTreesIn(Source.fromResource("tree-greed2").getLines().toVector) == 1779)
  }

  "The highest scenic score for a tree in the tree-greed1 file " should " be 8 " in {
    assert(TreeTopTreeHouse.getHighestScenicScoreForATreeIn(Source.fromResource("tree-greed1").getLines().toVector) == 8)
  }

  "The highest scenic score for a tree in the tree-greed2 file " should " be 172224 " in {
    assert(TreeTopTreeHouse.getHighestScenicScoreForATreeIn(Source.fromResource("tree-greed2").getLines().toVector) == 172224)
  }
}
