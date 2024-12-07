import common.loadPackets

val input = loadPackets(List("day07.txt"))
  .map({ case s"${sum}: ${rest}" =>
    (sum.toLong, rest.split("""\s+""").map(_.toLong).toList.reverse)
  })

def canBeTrue(result: Long, operands: List[Long]): Boolean = operands match {
  case List(x) => x == result
  case op :: rest if result % op == 0 => canBeTrue(result / op, rest) || canBeTrue(result - op, rest)
  case op :: rest => canBeTrue(result - op, rest)
}

val part1 = input.filter(canBeTrue.apply).map(_._1).sum

def reverseConcat(result: Long, operand: Long): Option[Long] = {
  val opString = operand.toString
  Some(result.toString)
    .filter(_.endsWith(opString))
    .map(_.stripSuffix(opString))
    .filter(_ != "-")
    .map {
      case "" => 0L
      case other => other.toLong
    }
}

def reverseMul(result: Long, operand: Long): Option[Long] =
  if result % operand == 0 then Some(result / operand) else None

def reverseAdd(result: Long, operand: Long): Option[Long] = Some(result - operand)

def canBeTrue2(result: Long, operands: List[Long]): Boolean = operands match {
  case List(x) => x == result
  case op :: rest =>
    List(reverseConcat, reverseMul, reverseAdd)
      .exists(reverseOperator => reverseOperator(result, op).exists(canBeTrue2(_, rest)))
}

val part2 = input.filter(canBeTrue2.apply).map(_._1).sum
// 156991927741 is too low

