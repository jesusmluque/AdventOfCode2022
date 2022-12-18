object DistressSignal:

  sealed trait NestedList
  final case class Elem(s:Int) extends NestedList
  final case class L(l: List[NestedList]) extends NestedList

  def rightOrderIndicesSumFor(rawPackets: List[String]) =
    val packets = buildPackets(rawPackets)
    val res = packets.zipWithIndex.filter(p => isRightOrder(p._1))
    res.foldLeft(0) { (acc, p) =>
      p._2 + 1 + acc
    }

  def calculateDecoderKey(rawPackets: List[String]) =
    val packetComparator = new Ordering[NestedList] {
      override def compare(x: NestedList, y: NestedList): Int = DistressSignal.compare(x, y)
    }
    val packets = (rawPackets ++ List("[[2]]", "[[6]]")).filter(_ != "").map(parse)
      .sorted(packetComparator).zipWithIndex
    val divider1 = packets.find(p => p._1 == parse("[[2]]"))
    val divider2 = packets.find(p => p._1 == parse("[[6]]"))
    divider1.flatMap(d1 => divider2.map(d2 => (d1._2 + 1) * (d2._2 + 1))).getOrElse(0)

  private def buildPackets(rawPackets: List[String]) = {
    val packets = rawPackets.filter(_ != "").grouped(2).map(rawPacket => (rawPacket.head, rawPacket.tail.head)).toList
    packets
  }

  private def isRightOrder(packetPair: (String, String)): Boolean =
    val p1 = parse(packetPair._1)
    val p2 = parse(packetPair._2)
    compare(p1, p2) == -1

  private def compare(first: NestedList, second:NestedList):Int =
    (first, second) match
      case (L(List()), _) => -1
      case (_, L(List())) => 1
      case (L(List()), L(List())) => 0
      case (f@Elem(_), s@Elem(_)) =>
        if f.s > s.s then 1 else if f.s == s.s then 0 else -1
      case (f@Elem(_), s:L) =>
        compare(L(List(f)), s)
      case (f:L, s@Elem(_)) =>
        compare(f, L(List(s)))
      case (f@L(_), s@L(_)) =>
        val dropUndecided = f.l.zip(s.l).dropWhile { case (c1, c2) => compare(c1, c2) == 0 }
        if dropUndecided.isEmpty then f.l.size.compareTo(s.l.size)
        else compare(dropUndecided.head._1, dropUndecided.head._2)

  private def parse(s: String):L =
    def buildList(s: String, acc:L):(L, String) =
      if s.head == '[' then
        val r = buildList(s.tail, L(List()))
        val res = L(r._1 :: acc.l)
        if r._2.nonEmpty then
          buildList(r._2, res)
        else
          (L(res.l.reverse), r._2)
      else if s.span(_.isDigit)._1.nonEmpty then
        buildList(s.span(_.isDigit)._2, L(Elem(s.span(_.isDigit)._1.toInt) :: acc.l))
      else if s.head == ']' then
        (L(acc.l.reverse), s.tail)
      else if s.span(_ == ',')._1.nonEmpty then
        buildList(s.span(_ == ',')._2, acc)
      else
        (acc, "")
    val res = buildList(s, L(List()))
    res._1.l.head match {
      case p:L => p
    }