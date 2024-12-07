import common.loadPackets

val input = loadPackets(List("day07.txt"))
  .map({ case s"${sum}: ${rest}" =>
    (sum.toLong, rest.split("""\s+""").map(_.toLong).toList.reverse)
  })

def reverseAdd(result: Long, operand: Long): Option[Long] = Some(result - operand)
def reverseMul(result: Long, operand: Long): Option[Long] =
  if result % operand == 0 then Some(result / operand) else None
def reverseConcat(result: Long, operand: Long): Option[Long] =
  if result == operand then Some(0)
  else if result.sign != operand.sign then None
  else {
    val opString = operand.toString
    Some(result.toString)
      .filter(_.endsWith(opString))
      .map(_.stripSuffix(opString).toLong)
  }

case class Algebra(reverseOperators: List[(Long, Long) => Option[Long]]):
  def canBeTrue(result: Long, operands: List[Long]): Boolean = operands match {
    case List(x) => x == result
    case op :: rest => reverseOperators.exists(reverse => reverse(result, op).exists(canBeTrue(_, rest)))
  }
  def calibrationResult: Long = input.filter({
    case (result, operands) => canBeTrue(result, operands)
  }).map(_._1).sum

val part1 = Algebra(List(reverseAdd, reverseMul)).calibrationResult
val part2 = Algebra(List(reverseAdd, reverseMul, reverseConcat)).calibrationResult