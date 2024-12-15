import common.loadPackets

val input = loadPackets(List("day15.txt"))
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

  def onGrid: Boolean = xs.contains(x) && ys.contains(y)
  def gps: Int = 100 * y + x

case class State(robot: Point, stones: Set[Point]):
  def move(instruction: Char): State = Some(robot.move(instruction)).filter(_.onGrid) match {
    case None => this
    case Some(moved) if moved.charAt == '#' => this
    case Some(moved) if !stones.contains(moved) => copy(robot = robot.move(instruction))
    case Some(moved) => push(instruction) match {
      case None => this
      case Some(pushed) => pushed.move(instruction)
    }
  }
  def charAt(p: Point): Option[Char] =
    if !p.onGrid then None
    else if p == robot then Some('@')
    else if stones.contains(p) then Some('O')
    else if p.charAt == '#' then Some('#')
    else Some('.')
  def draw: String = "\n" + ys.map(y => xs.map(x => charAt(Point(x, y)).get).mkString).mkString("\n")
  def push(instruction: Char): Option[State] =
    LazyList.iterate(robot)(_.move(instruction))
      .drop(1)
      .takeWhile(_.onGrid)
      .takeWhile(_.charAt != '#')
      .dropWhile(stones.contains)
      .headOption
    .map(target => copy(stones = stones.filterNot(_ == robot.move(instruction)) + target))

val points = for
  x <- xs
  y <- ys
yield Point(x, y)

val start = State(
  robot = points.find(_.charAt == '@').get,
  stones = points.filter(_.charAt == 'O').toSet)

val part1 = moves.foldLeft(start)((state, instruction) => state.move(instruction)).stones.map(_.gps).sum
