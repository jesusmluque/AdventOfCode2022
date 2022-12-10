object CathodeRayTube:

  def getSumSygnalStrengthAfterInstruction(rawInstruction: List[String]) =
    val pattern = "addx (-*[0-9]+)".r
    val result = calculateInstructions(rawInstruction)
    drawCRT(result)
    20*result(19) + 60*result(59) + 100*result(99) + 140*result(139) + 180*result(179) + 220*result(219)

  def calculateInstructions(instructions: List[String]) =
    val pattern = "addx (-*[0-9]+)".r
    instructions.foldLeft(List(1L)) { (acc, ins) => ins match {
      case "noop" => acc.head :: acc
      case pattern(value) => (value.toLong + acc.head) :: acc.head :: acc
    }
    }.reverse.toVector

  def drawCRT(register5History: Vector[Long]) =
    def drawRow(range: Range) =
      range.foreach { cycle =>
        val currentValue = register5History(cycle)
        if cycle + 1 - range.head < currentValue + 3 && cycle + 1 - range.head >= currentValue then
          System.out.print("#")
        else
          System.out.print("·")
      }
    drawRow(0 to 39)
    System.out.print("\n")
    drawRow(40 to 79)
    System.out.print("\n")
    drawRow(80 to 119)
    System.out.print("\n")
    drawRow(120 to 159)
    System.out.print("\n")
    drawRow(160 to 199)
    System.out.print("\n")
    drawRow(200 to 239)