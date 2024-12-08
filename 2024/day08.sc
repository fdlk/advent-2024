import common.loadPackets

val input = loadPackets(List("day08.txt"))
val ys = input.indices
val xs = input.head.indices

case class Point(x: Int, y: Int):
  def plus(d: Point, steps: Int = 1): Point = Point(x = x + d.x * steps, y = y + d.y * steps)
  def charAt: Char = input(y).charAt(x)

val points = for
  y <- ys
  x <- xs
yield Point(x, y)

val antennas = points.filter(_.charAt != '.')
val antennasByChar = antennas.groupBy(_.charAt)

def isAntinode(candidate: Point, antenna: Point, factors: Seq[Int] = Seq(1)): Boolean =
  antennasByChar(antenna.charAt)
    .filter(_ != antenna)
    .map(other => antenna.plus(other, -1))
    .flatMap(diff => factors.map(factor => antenna.plus(diff, factor)))
    .contains(candidate)

val part1 = points.count(candidate => antennas.exists(antenna => isAntinode(candidate, antenna)))

val factors = (-xs.last - ys.last) to (xs.last + ys.last)
val part2 = points.count(candidate => antennas.exists(antenna => isAntinode(candidate, antenna, factors)))