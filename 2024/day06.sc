import Direction.{East, North, South, West}
import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day06.txt"))
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

  def charAt: Char = input(y).charAt(x)

val yStart = input.indexWhere(_.contains('^'))
val xStart = input(yStart).indexOf('^')

case class State(droid: Point = Point(xStart, yStart), facing: Direction = Direction.North):
  def turnRight: State = facing match {
    case Direction.North => copy(facing = Direction.East)
    case Direction.East => copy(facing = Direction.South)
    case Direction.South => copy(facing = Direction.West)
    case Direction.West => copy(facing = Direction.North)
  }
  def move: Option[State] = Some(copy(droid = droid.move(facing)))
      .filter(_.droid.onGrid) match {
    case None => None
    case Some(moved) if moved.droid.charAt == '#' => Some(turnRight)
    case x => x
  }

LazyList.iterate[Option[State]](Some(State()))(_.flatMap(_.move))
  .takeWhile(_.nonEmpty).map(_.get.droid).toSet.size
