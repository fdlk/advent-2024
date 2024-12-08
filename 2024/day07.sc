import common.loadPackets

val input = loadPackets(List("day07.txt"))
  .map({ case s"${sum}: ${rest}" =>
    (BigInt(sum), rest.split("""\s+""").map(BigInt(_)).toList)
  })

case class Algebra(operators: List[(BigInt, BigInt) => BigInt]):
  def canBeTrue(desiredResult: BigInt, operands: List[BigInt], soFar: BigInt = 0): Boolean = 
    if soFar > desiredResult then false else operands match {
    case Nil => desiredResult == soFar
    case operand :: rest => operators.exists(op => canBeTrue(desiredResult, rest, op(soFar, operand)))
  }
  def calibrationResult: BigInt = input.filter({
    case (result, operands) => canBeTrue(result, operands)
  }).map(_._1).sum

def concat(a: BigInt, b: BigInt) = BigInt(s"${a}${b}")

val part1 = Algebra(List(_+_, _*_)).calibrationResult
val part2 = Algebra(List(_+_, _*_, concat)).calibrationResult