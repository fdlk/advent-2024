import common.loadPackets

val input = loadPackets(List("day07.txt"))
  .map({ case s"${sum}: ${rest}" =>
    (sum.toLong, rest.split("""\s+""").map(_.toLong).toList.reverse) })

def canBeTrue(result: Long, operands: List[Long]): Boolean = operands match {
  case List(x) => x == result
  case op :: rest if result % op == 0 => canBeTrue(result / op, rest) || canBeTrue(result - op, rest)
  case op :: rest => canBeTrue(result - op, rest)
}

val part1 = input.filter(canBeTrue.apply).map(_._1).sum

