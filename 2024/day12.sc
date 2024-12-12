import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day12.txt"))
val ys = input.indices
val xs = input.head.indices

enum Direction:
  case Up, Right, Down, Left

case class Point(x: Int, y: Int):
  def onGrid: Boolean = xs.contains(x) && ys.contains(y)

  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

  def move(d: Direction): Point = d match {
    case Direction.Up => plus(Point(0, -1))
    case Direction.Right => plus(Point(1, 0))
    case Direction.Down => plus(Point(0, 1))
    case Direction.Left => plus(Point(-1, 0))
  }

  def neighbors: Seq[Point] = Direction.values.toSeq.map(move)

  def fences: Set[List[Point]] = neighbors.filter(_.charAt != charAt).toSet.map(neighbor => List(neighbor, this))

  def charAt: Option[Char] = if onGrid then Some(input(y)(x)) else None

@tailrec
def floodFill(points: Set[Point]): Set[Point] = {
  val char = points.head.charAt
  val updated = points ++ points.flatMap(_.neighbors)
    .filter(_.charAt == char)
  if updated == points then points else floodFill(updated)
}

val points = for
  y <- ys
  x <- xs
yield Point(x, y)

def perimeter(island: Set[Point]): Int = island.flatMap(_.fences).size

def score(island: Set[Point]): Int = perimeter(island) * island.size

val islands = points.map(Set(_)).map(floodFill).distinct

val part1 = islands.map(score).sum

def findSublist(l: List[Int]): List[Int] =
  l.sliding(2).takeWhile(pair => pair.head + 1 == pair(1)).flatMap(_.toList).distinct.toList

def findSublists(l: List[Int]): List[List[Int]] =
  if l.isEmpty then Nil
  else if l.size == 1 then List(l)
  else {
    val sublist = findSublist(l)
    if sublist.isEmpty
    then List(l.head) :: findSublists(l.tail)
    else sublist :: findSublists(l.drop(sublist.length))
  }

def sides(island: Set[Point]): Int =
  val fences = island.flatMap(_.fences)
  fences.filter(_.map(_.x).distinct.size == 1)
    .groupBy(_.map(_.y))
    .map((_, xs) => findSublists(xs.flatMap(_.map(_.x).toSet).toList.sorted).size).sum
    + fences.filter(_.map(_.y).distinct.size == 1)
    .groupBy(_.map(_.x))
    .map((_, ys) => findSublists(ys.flatMap(_.map(_.y).toSet).toList.sorted).size).sum

val part2 = islands.map(island => island.size * sides(island)).sum