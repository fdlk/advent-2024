import common.{Grid, aStarSearch, binarySearch, loadPackets}

import scala.annotation.tailrec

val gridSize = 70
val xs = 0 to gridSize
val ys = 0 to gridSize

enum Direction:
  case Up, Right, Down, Left

case class Point(x: Int, y: Int):
  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)
  def move(d: Direction): Point = d match {
    case Direction.Up => plus(Point(0, -1))
    case Direction.Right => plus(Point(1, 0))
    case Direction.Down => plus(Point(0, 1))
    case Direction.Left => plus(Point(-1, 0))
  }
  def neighbors: Seq[Point] = Direction.values.toSeq
    .map(move)
    .filter(_.onGrid)
  def distance(other: Point) = (other.x - x).abs.max((other.y - y).abs)

val start = Point(0, 0)
val end = Point(gridSize, gridSize)
start.neighbors

val corrupted: Seq[Point] = loadPackets(List("day18.txt")).map({
  case s"${x},${y}" => Point(x.toInt, y.toInt)
})

def grid(corrupted: Set[Point]): Grid[Point] = new Grid[Point]:
  override def heuristicDistanceToFinish(from: Point): Int = from.distance(end)
  override def getNeighbours(state: Point): Iterable[Point] = state.neighbors.filterNot(corrupted.contains)
  override def moveCost(from: Point, to: Point): Int = from.distance(to)

def findPath(t: Int) = aStarSearch(start, grid(corrupted.take(t).toSet), _ == end)

val (part1, _) = findPath(1024).get

val t = binarySearch(corrupted.indices, findPath(_).isDefined)
val part2 = corrupted(t)