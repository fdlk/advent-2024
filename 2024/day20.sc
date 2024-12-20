import common.{Grid, aStarSearch, loadPackets}

import scala.annotation.tailrec

val input = loadPackets(List("day20.txt"))

val ys = input.indices
val xs = input.head.indices

enum Direction:
  case North, East, South, West

case class Point(x: Int, y: Int):
  def charAt: Char = input(y).charAt(x)

  def move(d: Direction) = d match {
    case Direction.North => copy(y = y - 1)
    case Direction.East => copy(x = x + 1)
    case Direction.South => copy(y = y + 1)
    case Direction.West => copy(x = x - 1)
  }

  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def distanceTo(other: Point): Int = (other.x - x).abs + (other.y - y).abs

  def isWall: Boolean = charAt == '#'

  def neighbors: Iterable[Point] = Direction.values.toSeq.map(move).filterNot(_.isWall)

val points = for
  y <- ys
  x <- xs
yield Point(x, y)

val start = points.find(_.charAt == 'S').get
val end = points.find(_.charAt == 'E').get


val grid: Grid[Point] = new Grid[Point] {
  override def heuristicDistanceToFinish(from: Point): Int = 0
  override def getNeighbours(Point: Point): Iterable[Point] = Point.neighbors
  override def moveCost(from: Point, to: Point): Int = 1
}

val (pathSize, distances): (Int, Map[Point, Int]) = aStarSearch(start, grid, _ == end).get

@tailrec
def findPath(state: Point, soFar: List[Point] = Nil): List[Point] =
  if state == start then start :: soFar
  else {
    val next: Point = state.neighbors
      .filter(distances.contains)
      .find(neighbor => distances(neighbor) + grid.moveCost(neighbor, state) == distances(state))
      .get
    findPath(next, state :: soFar)
  }

val path = findPath(end).toVector

val cheats = for
  from <- path.indices
  to <- from + 100 until path.indices.end
  distance = path(from).distanceTo(path(to))
  if distance <= 2
  profit = to - from - distance
  if profit >= 100
yield profit


val part1 = cheats.count(_ >= 100)

val newcheats = for
  from <- path.indices
  to <- from + 100 until path.indices.end
  distance = path(from).distanceTo(path(to))
  if distance <= 20
  profit = to - from - distance
  if profit >= 100
yield profit


val part2 = newcheats.count(_ >= 100)
