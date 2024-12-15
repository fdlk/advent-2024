import common.loadPackets

val input = loadPackets(List("day15-test.txt"))
val (mapList, movesList) = input.splitAt(input.indexWhere(_.isEmpty))
val moves = movesList.mkString

val ys = mapList.indices
val xs = mapList.head.indices

case class Point(x: Int, y: Int):
  def plus(d: Point, steps: Int = 1): Point = Point(x = x + d.x * steps, y = y + d.y * steps)

  def charAt: Char = mapList(y).charAt(x)

  def move(instruction: Char) = instruction match {
    case '^' => copy(y = y - 1)
    case '>' => copy(x = x + 1)
    case 'v' => copy(y = y + 1)
    case '<' => copy(x = x - 1)
  }

case class State(robot: Point, stones: Set[Point])

val points = for
  x <- xs
  y <- ys
yield Point(x, y)

val start = State(
  robot = points.find(_.charAt == '@').get,
  stones = points.filter(_.charAt == 'O').toSet)

