object SupplyStacks {

  case class Instruction(amount: Int, from: Int, to: Int)
  object Instruction {
    def apply(raw: String):Instruction =
      val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
      val pattern(a, f, t) = raw
      Instruction(a.toInt,f.toInt,t.toInt)
  }

  case class Stacks(private val stacks: Map[Int, List[String]]) {
    def move(i: Instruction, model: String = "9000") =
      Stacks(stacks.updated(i.from, stacks(i.from).drop(i.amount)) //Drop from origin
                   .updated(i.to, (if model == "9000" then stacks(i.from).take(i.amount).reverse else stacks(i.from).take(i.amount)) ++ stacks(i.to)))  //add to the destination
    def getTop = stacks.toSeq.sortBy(_._1).map(_._2.headOption.getOrElse("")).toList
  }
  object Stacks {
    def apply(raw: List[String]): Stacks = {
      val rawIndexes = raw.reverse.head
      val rawValues = raw.reverse.tail
      val indexes = rawIndexes.foldLeft(List[(Int, Int)]()) { (acc, c) =>
        if c != ' ' then (c.toInt - 48, rawIndexes.indexOf(c)) :: acc else acc}.reverse

      Stacks(indexes.foldLeft(Map[Int, List[String]]()) { (acc, r) =>
        acc.updated(r._1, rawValues.foldLeft(List[String]()) { (a, row) =>
          if r._2 < row.length && row(r._2) != ' ' then row(r._2).toString :: a else a })
      })
    }
  }

  def getCratesFromTopOfStacksAfterInstructionApplied(rawData: List[String], model:String = "9000"):String =
    val (stacks, instructions) = parse(rawData)
    instructions.foldLeft(stacks) { (acc, n) => model match
      case "9000" => acc.move(n)
      case "9001" => acc.move(n, "9001")
    }.getTop.mkString("")

  def parse(raw: List[String]) =
    val (rawStacks, rawInstructions) = raw.splitAt(raw.indexOf(""))
    (Stacks(rawStacks), rawInstructions.tail.map(Instruction(_)))
}