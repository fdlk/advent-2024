import common.loadPackets

val input = loadPackets(List("day03.txt")).mkString

val part1 =
  """mul\(\d{1,3},\d{1,3}\)""".r
    .findAllIn(input)
    .map({
      case s"mul($x,$y)" => x.toInt * y.toInt
    }).sum

case class State(sum: Int = 0, doing: Boolean = true)

val part2 =
  """mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)""".r
    .findAllIn(input)
    .foldLeft(State())({
      case (State(soFar, true), s"mul($x,$y)") => State(sum = soFar + x.toInt * y.toInt)
      case (s, "don't()") => s.copy(doing = false)
      case (s, "do()") => s.copy(doing = true)
      case (s, _) => s
    }).sum
