import common.{Grid, aStarSearch, loadPackets}

val input = loadPackets(List("day16.txt"))

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

val points = for
  y <- ys
  x <- xs
yield Point(x, y)

val start = points.find(_.charAt == 'S').get
val end = points.find(_.charAt == 'E').get

case class State(location: Point = start, facing: Direction = Direction.East):
  def neighbors: Iterable[State] = Direction.values.toSeq.map(d => State(location.move(d), d))
    .filter(_.location.charAt != '#')

val grid: Grid[State] = new Grid[State] {
  override def heuristicDistanceToFinish(from: State): Int = end.distanceTo(from.location)
  override def getNeighbours(state: State): Iterable[State] = state.neighbors
  override def moveCost(from: State, to: State): Int = 1 + (if from.facing != to.facing then 1000 else 0)
}

aStarSearch(State(), grid, _.location == end)