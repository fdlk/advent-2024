import common.loadPackets

val input = loadPackets(List("day04.txt")).toArray

val xs = input.indices
val ys = input.head.indices

case class Point(x: Int, y: Int):
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

  def charAt: Char = input(y).charAt(x)

case object Points:
  def directions = for
    dx <- -1 to 1
    dy <- -1 to 1
    if dx != 0 || dy != 0
  yield Point(dx, dy)

def getLine(start: Point, direction: Point, length: Int): String =
  (0 until length)
    .map(index => start.plus(direction, index))
    .filter(_.onGrid)
    .map(_.charAt)
    .mkString

def isXMASLine(start: Point, direction: Point) =
  getLine(start, direction, 4) == "XMAS"

val part1 = (for
  x <- xs
  y <- ys
  start = Point(x, y)
  d <- Points.directions
  if isXMASLine(start, d)
yield start).size

def isXMASCross(start: Point): Boolean =
  List("MAS", "SAM").contains(getLine(start.plus(Point(-1, -1)), Point(1, 1), 3)) &&
  List("MAS", "SAM").contains(getLine(start.plus(Point(-1, 1)), Point(1, -1), 3))

val part2 = (for
  x <- xs
  y <- ys
  start = Point(x, y)
  if isXMASCross(start)
yield start).size