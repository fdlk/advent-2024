import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day12.txt"))
val ys = input.indices
val xs = input.head.indices

enum Direction:
  case Up, Right, Down, Left

val corners = for
  vert <- List(Direction.Up, Direction.Down)
  hor <- List(Direction.Left, Direction.Right)
yield (hor, vert)

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
  def fences = neighbors.filter(_.charAt != charAt).map(neighbor => List(this, neighbor))
  def isCorner(hor: Direction, vert: Direction): Boolean = (move(hor).charAt, move(vert).charAt, move(hor).move(vert).charAt) match {
    case (h, v, _) if h != charAt && v != charAt => true  
    case (h, v, hv) if h == charAt && v == charAt && hv != charAt => true
    case _ => false
  }
  def getCorners: Int = corners.count(isCorner)

@tailrec
def floodFill(points: Set[Point]): Set[Point] = {
  val char = points.head.charAt
  val updated = points ++ points.flatMap(_.neighbors).filter(_.charAt == char)
  if updated == points then points else floodFill(updated)
}

def perimeter(island: Set[Point]): Int = island.flatMap(_.fences).size
def score(island: Set[Point]): Int = perimeter(island) * island.size

def makeIslands(points: Seq[Point]): List[Set[Point]] = 
  if points.isEmpty then Nil
  else {
    val island = floodFill(Set(points.head))
    island :: makeIslands(points.filterNot(island.contains))
  }

val points = for
  y <- ys
  x <- xs
yield Point(x, y)
val islands = makeIslands(points)
val part1 = islands.map(score).sum

def sides(island: Set[Point]): Int = island.toList.map(_.getCorners).sum
val part2 = islands.map(island => island.size * sides(island)).sum