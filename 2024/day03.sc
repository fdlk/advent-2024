import common.loadPackets

val input = loadPackets(List("day03.txt")).mkString

val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r
val instructionRegex = """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r

val instructions = instructionRegex.findAllIn(input).toList

val part1 = instructions.map({
  case mulRegex(x, y) => x.toLong * y.toLong
  case _ => 0
}).sum

case class State(sum: Int = 0, doing: Boolean = true)

val part2 = instructions.foldLeft(State())({
  case (State(s, true), mulRegex(x, y)) => State(sum = s + x.toInt * y.toInt)
  case (s, "don't()") => s.copy(doing = false)
  case (s, "do()") => s.copy(doing = true)
  case (s, _) => s
}).sum