import scala.annotation.tailrec

object MonkeyInTheMiddle:

  case class Operation(typeOfOperation: String, value: String):
    def execute(oldVal: Long) = typeOfOperation match
      case "*" => oldVal * (if value == "old" then oldVal else value.toLong)
      case "+" => oldVal + (if value == "old" then oldVal else value.toLong)
  object Operation:
    def apply(rawOp: String):Operation =
      val patternOperation = " new = old ([\\*\\+]{1}) ([0-9]+|old)".r
      val patternOperation(o, v) = rawOp
      Operation(o, v)

  case class Monkey(monkeyName: String, startingItems: List[Long], operation: Operation, testValue: Long, action: (String, String), itemInspected: Int = 0)
  object Monkey:
    def apply(rawMonkey: List[String]):Monkey =
      val namePattern = "Monkey ([0-9]+):".r
      val testValuePattern = " divisible by ([0-9]+)".r
      val actionPattern = " throw to monkey ([0-9]+)".r
      val namePattern(name) = rawMonkey(0)
      val startingItems = rawMonkey(1).split(":")(1).split(",").map(v => v.trim.toLong).toList
      val operation = Operation(rawMonkey(2).split(":")(1))
      val testValuePattern(v) = rawMonkey(3).split(":")(1)
      val actionPattern(trueAction) = rawMonkey(4).split(":")(1)
      val actionPattern(falseAction) = rawMonkey(5).split(":")(1)
      Monkey(name, startingItems, operation, v.toLong, (trueAction, falseAction))
    @tailrec
    def throwItems(from: Monkey, monkeys: Map[String, Monkey], f: Long => Long):Map[String, Monkey] =
      from.startingItems match
        case List() => monkeys
        case item :: rest =>
          val worryLevel = from.operation.execute(item)
          val worryLevelBored = f(worryLevel)
          val to = if worryLevelBored % from.testValue == 0 then monkeys(from.action._1) else monkeys(from.action._2)
          val newFrom = Monkey(from.monkeyName, from.startingItems.tail, from.operation, from.testValue, from.action, from.itemInspected + 1)
          val newTo = Monkey(to.monkeyName, to.startingItems :+ worryLevelBored, to.operation, to.testValue, to.action, to.itemInspected)
          throwItems(newFrom, monkeys.updated(from.monkeyName, newFrom).updated(newTo.monkeyName, newTo), f)

  def calculateMonkeyBusinessFor20Rounds(rawMonkeys: List[String]) =
    calculateMonkeyBusiness(buildMonkeys(rawMonkeys), 20, v => v / 3)

  def calculateMonkeyBusinessFor1000Rounds(rawMonkeys: List[String]) =
    val monkeys = buildMonkeys(rawMonkeys)
    val testValueMultiple = monkeys.foldLeft(1L)((acc, m) => acc * m._2.testValue)
    calculateMonkeyBusiness(buildMonkeys(rawMonkeys), 10000, v => v % testValueMultiple)

  private def buildMonkeys(rawMonkeys: List[String]) =
    rawMonkeys.filter(_ != "").grouped(6).map(Monkey(_)).map(m => (m.monkeyName, m)).toMap

  private def calculateMonkeyBusiness(monkeys: Map[String, Monkey], round: Int, worryCorrection: Long => Long) =
    (1 to round).foldLeft(monkeys) { (acc, _) =>
      (0 until acc.size).foldLeft(acc) {(a, index) =>
        Monkey.throwItems(a(index.toString), a, worryCorrection)
      }
    }.values.toList.map(_.itemInspected.toLong).sorted(Ordering.Long.reverse).take(2).product