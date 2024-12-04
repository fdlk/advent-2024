import common.loadPackets

val input = loadPackets(List("day04.txt")).toArray

val xs = input.indices
val ys = input.head.indices

case class Point(x: Int, y: Int):
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def plus(d: Point, steps: Int): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

  def charAt: Char = input(y).charAt(x)

case object Points:
  def directions = for
    dx <- -1 to 1
    dy <- -1 to 1
    if dx != 0 || dy != 0
  yield Point(dx, dy)

def isXMAS(start: Point, direction: Point) =
  "XMAS".indices
    .map(index => start.plus(direction, index))
    .filter(_.onGrid)
    .map(_.charAt)
    .mkString == "XMAS"

val part1 = (for
  x <- xs
  y <- ys
  start = Point(x, y)
  d <- Points.directions
  if isXMAS(start, d)
yield start).size