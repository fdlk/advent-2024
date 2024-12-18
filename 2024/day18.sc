import common.{Grid, aStarSearch, loadPackets}

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

def grid(t: Int): Grid[Point] = new Grid[Point]:
  override def heuristicDistanceToFinish(from: Point): Int = from.distance(end)
  override def getNeighbours(state: Point): Iterable[Point] = state.neighbors.filterNot(corrupted.take(t).contains(_))
  override def moveCost(from: Point, to: Point): Int = from.distance(to)

val part1 = aStarSearch(start, grid(1024), _ == end)

@tailrec
def search(range: Range, pred: Int => Boolean): Int =
  if range.size == 1
  then range.start
  else
    val mid: Int = range.start + 1 + (range.end - range.start) / 2
    search(if pred(mid) then mid to range.end else range.start until mid, pred)

val tBlock = search(corrupted.indices, t => aStarSearch(start, grid(t), _ == end).isDefined)
val part2 = corrupted(tBlock)
