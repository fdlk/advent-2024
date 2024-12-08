import common.loadPackets

val input = loadPackets(List("day08.txt"))
val ys = input.indices
val xs = input.head.indices

case class Point(x: Int, y: Int):
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

  def charAt: Char = input(y).charAt(x)

val points = for
  y <- ys
  x <- xs
  p = Point(x, y)
yield p

val antennas = points.filter(_.charAt != '.')

val antennasByChar = antennas.groupBy(_.charAt)

def isAntinode(candidate: Point, antenna: Point): Boolean = {
  val diff = antenna.plus(candidate, -1)
  val opposite = candidate.plus(diff, 2)
  antenna != candidate && opposite.onGrid && antennasByChar(antenna.charAt).contains(opposite)
}

val part1 = points.count(candidate =>
  antennas.exists(antenna => isAntinode(candidate, antenna)))