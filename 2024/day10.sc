import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day10.txt")).map(_.map(_.toString.toInt))
val ys = input.indices
val xs = input.head.indices

enum Direction:
  case North, East, South, West

case class Point(x: Int, y: Int):
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

  def move(d: Direction): Point = d match {
    case Direction.North => plus(Point(0, -1))
    case Direction.East => plus(Point(1, 0))
    case Direction.South => plus(Point(0, 1))
    case Direction.West => plus(Point(-1, 0))
  }

  def height: Int = input(y)(x)

  def neighbors: Seq[Point] = Direction.values
    .map(move)
    .filter(_.onGrid)
    .filter(_.height == height + 1)

val points = for
  y <- ys
  x <- xs
yield Point(x, y)

val trailheads = points.filter(_.height == 0)

def score(point: Point): Set[Point] =
  if point.height == 9
  then Set(point)
  else point.neighbors.toSet.flatMap(score)

val part1 = trailheads.map(score).map(_.size).sum

def rating(point: Point): Int =
  if point.height == 9
  then 1
  else point.neighbors.map(rating).sum

val part2 = trailheads.map(rating).sum