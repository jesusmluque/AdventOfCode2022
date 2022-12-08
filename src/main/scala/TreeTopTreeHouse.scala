object TreeTopTreeHouse:

  def countVisibleTreesIn(rawGrid: Vector[String]) =
    def isVisibleInSameRowLeft(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (columnIndex - 1 to 0 by -1).foldLeft((true, false))((acc, col) => if treeValue.toInt > rawGrid(rowIndex)(col).toInt && !acc._2 then (true, acc._2) else (false, true))._1
    def isVisibleInSameRowRight(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (columnIndex + 1 until rawGrid.size by 1).foldLeft((true, false))((acc, col) => if treeValue.toInt > rawGrid(rowIndex)(col).toInt && !acc._2 then (true, acc._2) else (false, true))._1
    def isVisibleInSameColumnUp(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (rowIndex - 1 to 0 by -1).foldLeft((true, false))((acc, row) => if treeValue.toInt > rawGrid(row)(columnIndex).toInt && !acc._2 then (true, acc._2) else (false, true))._1
    def isVisibleInSameColumnDown(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (rowIndex + 1 until rawGrid(0).length).foldLeft((true, false))((acc, row) => if treeValue.toInt > rawGrid(row)(columnIndex).toInt && !acc._2 then (true, acc._2) else (false, true))._1
    rawGrid.zipWithIndex.map { row =>
      row._1.toVector.zipWithIndex.map { tree => val (a, b) = (row._2, tree._2)
        isVisibleInSameRowRight(a, b, tree._1) || isVisibleInSameRowLeft(a, b, tree._1) || isVisibleInSameColumnUp(a, b, tree._1) || isVisibleInSameColumnDown(a, b, tree._1)
      }
    }.map(_.count(a => a)).sum

  def getHighestScenicScoreForATreeIn(rawGrid: Vector[String]):Int =
    def countTreesFromCurrentPositionLeft(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (columnIndex - 1 to 0 by -1).foldLeft((0, false))((acc, col) => if treeValue.toInt > rawGrid(rowIndex)(col).toInt && !acc._2 && !acc._2 then (acc._1 + 1, acc._2) else if acc._2 then (acc._1, true) else (acc._1 + 1, true))._1
    def countTreesFromCurrentPositionRight(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (columnIndex + 1 until rawGrid.size by 1).foldLeft((0, false))((acc, col) => if treeValue.toInt > rawGrid(rowIndex)(col).toInt && !acc._2 then (acc._1 + 1, acc._2) else if acc._2 then (acc._1, true) else (acc._1 + 1, true))._1
    def countTreesFromCurrentPositionUp(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (rowIndex - 1 to 0 by -1).foldLeft((0, false))((acc, row) => if treeValue.toInt > rawGrid(row)(columnIndex).toInt && !acc._2 then (acc._1 + 1, acc._2) else if acc._2 then (acc._1, true) else (acc._1 + 1, true))._1
    def countTreesFromCurrentPositionDown(rowIndex: Int, columnIndex: Int, treeValue: Char) =
      (rowIndex + 1 until rawGrid(0).length).foldLeft((0, false))((acc, row) => if treeValue.toInt > rawGrid(row)(columnIndex).toInt && !acc._2 then (acc._1 + 1, acc._2) else if acc._2 then (acc._1, true) else (acc._1 + 1, true))._1
    rawGrid.zipWithIndex.map { row =>
      row._1.toVector.zipWithIndex.map { tree => val (a, b) = (row._2, tree._2)
      countTreesFromCurrentPositionRight(a, b, tree._1) * countTreesFromCurrentPositionLeft(a, b, tree._1) * countTreesFromCurrentPositionUp(a, b, tree._1) * countTreesFromCurrentPositionDown(a, b, tree._1)
      }
    }.map(_.max).max