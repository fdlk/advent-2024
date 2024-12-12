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

  def charAt: Option[Char] = if onGrid then Some(input(y)(x)) else None

  def fences = neighbors.filter(_.charAt != charAt).map(neighbor => Fence(this, neighbor))

  case class Fence(inside: Point, outside: Point):
    def isHorizontal: Boolean = inside.x == outside.x

    def inCommon: Int = if isHorizontal then inside.x else inside.y

    def between: (Int, Int) = if isHorizontal then (inside.y, outside.y) else (inside.x, outside.x)

@tailrec
def floodFill(points: Set[Point]): Set[Point] = {
  val char = points.head.charAt
  val updated = points ++ points.flatMap(_.neighbors).filter(_.charAt == char)
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

def countAdjacentElementsFromHead(l: List[Int]): Int =
  l.sliding(2)
    .takeWhile({ case List(a, b) => a + 1 == b })
    .size + 1

def countSublists(l: List[Int]): Int =
  if l.isEmpty then 0
  else if l.size == 1 then 1
  else 1 + countSublists(l.drop(countAdjacentElementsFromHead(l)))

/**
 *  - Group the horizontal fences by the row numbers of the points inside and outside the fence.
 *  - Then for each fence in such a group determine the column numbers of the fence
 *  - Sort the column numbers. Each side is a group of adjacent column numbers.
 *  - Do the reverse for the vertical fences.
 *
 * ==Example==
 * An example counting horizontal fences of a U-shaped island:{{{
 *   01234
 *  0
 *    - -
 *  1 B B
 *  2 BBB
 *    ---
 *  3
 * }}}
 *
 * The top row of horizontal fences between rows `(1,0)` has list of column numbers `1,3`. Two sublists of size 1. So there are 2 sides.
 *
 * The bottom row of horizontal fences between rows `(2,3)` has list of column numbers `1,2,3`. One sublist of size 3. So there is 1 side.
 *
 * Vertical fences of the same island:{{{
 *   0 123 4
 *  0
 *  1 |B B|
 *  2 |BBB|
 *  3
 * }}}
 * The left column of vertical fences between columns `(1,0)` has list of row numbers `1,2`. One sublist of size 2. So there is 1 side.
 *
 * Right column of vertical fences between columns `(3,4)` has list of row numbers `1,2`. One sublist of size 2. So there is 1 side.
 */
def sides(island: Set[Point]): Int =
  island.flatMap(_.fences)
    .groupBy(fence => (fence.isHorizontal, fence.between))
    .values.map(_.map(_.inCommon))
    .map(_.toList.distinct.sorted)
    .map(countSublists)
    .sum

val part2 = islands.map(island => island.size * sides(island)).sum