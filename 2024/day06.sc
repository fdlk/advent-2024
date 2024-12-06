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

  def charAt(map: List[String] = input): Char = map(y).charAt(x)

val yStart = input.indexWhere(_.contains('^'))
val xStart = input(yStart).indexOf('^')

case class State(droid: Point = Point(xStart, yStart), facing: Direction = Direction.North, map: List[String] = input):
  def turnRight: State = facing match {
    case Direction.North => copy(facing = Direction.East)
    case Direction.East => copy(facing = Direction.South)
    case Direction.South => copy(facing = Direction.West)
    case Direction.West => copy(facing = Direction.North)
  }

  def move: Option[State] = Some(copy(droid = droid.move(facing)))
    .filter(_.droid.onGrid) match {
    case None => None
    case Some(moved) if moved.droid.charAt(map) == '#' => Some(turnRight)
    case x => x
  }

val patrolled = LazyList.iterate[Option[State]](Some(State()))(_.flatMap(_.move))
  .takeWhile(_.nonEmpty).map(_.get.droid).toSet
val part1 = patrolled.size

def placeObstacle(obstacle: Point): List[String] =
  input.zipWithIndex
    .map((row, y) =>
      if y == obstacle.y
      then row.zipWithIndex.map((c, x) =>
        if x == obstacle.x
        then '#'
        else c
      ).mkString
      else row
    )

def isLoop(map: List[String]) =
  LazyList.iterate[Option[State]](Some(State(map = map)))(_.flatMap(_.move))
    .takeWhile(_.nonEmpty)
    .map(_.get.copy(map = Nil))
    .scanLeft[(Set[State], Boolean)]((Set(), false))((soFar, state) => soFar match {
      case (visited, _) => (visited + state, visited.contains(state))
    })
    .exists(_._2)

val part2 = patrolled.map(placeObstacle).count(isLoop)