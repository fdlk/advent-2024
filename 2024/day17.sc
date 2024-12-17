import common.loadPackets

def parseProgram(program: String): List[Int] = program.split(",").map(_.toInt).toList

case class Computer(a: Int = 0, b: Int = 0, c: Int = 0, program: List[Int] = Nil, ip: Int = 0):
  def opcode: Int = program(ip)

  def literalOperand: Int = program(ip + 1)

  def comboOperand: Int = literalOperand match {
    case x if (0 to 3).contains(x) => x
    case 4 => a
    case 5 => b
    case 6 => c
  }

  val isHalted: Boolean = !program.indices.contains(ip + 1)

  def incrementIp = copy(ip = ip + 2)

  def next: Computer = opcode match {
    // adv
    case 0 => copy(a = Math.floor(a / Math.pow(2, comboOperand)).toInt).incrementIp
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
    case 5 =>
      print(comboOperand % 8);
      print(",")
      incrementIp
    // bdv
    case 6 => copy(b = Math.floor(a / Math.pow(2, comboOperand)).toInt).incrementIp
    // cdv
    case 7 => copy(c = Math.floor(a / Math.pow(2, comboOperand)).toInt).incrementIp
  }

  def run: Option[Computer] = LazyList.iterate(this)(_.next).find(_.isHalted)

  def run(program: String): Option[Computer] = copy(program = parseProgram(program)).run

val start = loadPackets(List("day17.txt")).match {
  case List(
  s"Register A: ${a}",
  s"Register B: ${b}",
  s"Register C: ${c}",
  "",
  s"Program: ${program}") =>
    Computer(a.toInt, b.toInt, c.toInt, parseProgram(program))
}

val part1 = start.run