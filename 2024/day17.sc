import common.loadPackets

import scala.annotation.tailrec

def parseProgram(program: String): List[Int] = program.split(",").map(_.toInt).toList

case class Computer(a: Long = 0, b: Long = 0, c: Long = 0, program: List[Int] = Nil, ip: Int = 0):
  def opcode: Int = program(ip)

  def literalOperand: Int = program(ip + 1)

  def comboOperand: Long = literalOperand match {
    case x if (0 to 3).contains(x) => x
    case 4 => a
    case 5 => b
    case 6 => c
  }

  val isHalted: Boolean = !program.indices.contains(ip + 1)

  def incrementIp = copy(ip = ip + 2)

  def output: Option[Int] = opcode match {
    case 5 => Some((comboOperand % 8).toInt)
    case _ => None
  }

  def next: Computer = opcode match {
    // adv
    case 0 => copy(a = a >> comboOperand).incrementIp
    // bxl
    case 1 => copy(b = b ^ literalOperand).incrementIp
    // bst
    case 2 => copy(b = comboOperand % 8).incrementIp
    // jnz
    case 3 if a == 0 => incrementIp
    case 3 => copy(ip = literalOperand)
    // bxc
    case 4 => copy(b = b ^ c).incrementIp
    // out
    case 5 => incrementIp
    // bdv
    case 6 => copy(b = a >> comboOperand).incrementIp
    // cdv
    case 7 => copy(c = a >> comboOperand).incrementIp
  }
  def run: LazyList[Int] = LazyList.iterate(this)(_.next).takeWhile(!_.isHalted).flatMap(_.output)

val computer = loadPackets(List("day17.txt")).match {
  case List(
  s"Register A: ${a}",
  s"Register B: ${b}",
  s"Register C: ${c}",
  "",
  s"Program: ${program}") =>
    Computer(a.toInt, b.toInt, c.toInt, parseProgram(program))
}

val part1 = computer.run.mkString(",")

@tailrec
def findNextDigit(prefixes: List[Long], length: Int, goal: List[Int] = computer.program): List[Long] = {
  if length > goal.length then prefixes
  else findNextDigit(prefixes.flatMap(prefix => (0 to 7).map(8 * prefix + _))
      .filter(a => computer.copy(a = a).run == goal.takeRight(length)), length + 1)
}

val part2 = findNextDigit(List(0L), 1, computer.program).min
