import common.loadPackets

val input = loadPackets(List("day15.txt"))
val (narrowMapList, movesList) = input.splitAt(input.indexWhere(_.isEmpty))
val moves = movesList.mkString
val mapList = narrowMapList.map(_. flatMap(_ match {
  case '#' => "##"
  case 'O' => "[]"
  case '.' => ".."
  case '@' => "@."
}))

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

case class State(robot: Point, packetLefts: Set[Point]):
  val packetRights: Set[Point] = packetLefts.map(_.move('>'))
  val packets: Set[Point] = packetLefts ++ packetRights
  def move(instruction: Char): State = Some(robot.move(instruction)).filter(_.onGrid) match {
    case None => this
    case Some(moved) if moved.charAt == '#' => this
    case Some(moved) if !packets.contains(moved) => copy(robot = robot.move(instruction))
    case Some(moved) => push(packetLeft(moved), instruction) match {
      case None => this
      case Some(pushed) => pushed.move(instruction)
    }
  }
  def charAt(p: Point): Option[Char] =
    if !p.onGrid then None
    else if p == robot then Some('@')
    else if packetLefts.contains(p) then Some('[')
    else if packetRights.contains(p) then Some(']')
    else if p.charAt == '#' then Some('#')
    else Some('.')
  def draw: String = "\n" + ys.map(y => xs.map(x => charAt(Point(x, y)).get).mkString).mkString("\n")
  def packetLeft(packet: Point) = if packetRights.contains(packet) then packet.move('<') else packet
  def push(left: Point, instruction: Char): Option[State] = {
    val right = left.move('>')
    val blockers: Set[Point] = instruction match {
      case '^' | 'v' => Set(left, right).map(_.move(instruction))
      case '>' => Set(right.move('>'))
      case '<' => Set(left.move('<'))
    }
    if blockers.exists(_.charAt == '#') then None
    else {
      blockers.map(packetLeft).intersect(packetLefts)
        .foldLeft[Option[State]](Some(this))((pushed, blocker) =>
          pushed.flatMap(_.push(blocker, instruction)))
        .map(moved => moved.copy(packetLefts = moved.packetLefts - left + left.move(instruction)))
    }
  }

val points = for
  x <- xs
  y <- ys
yield Point(x, y)

val start = State(
  robot = points.find(_.charAt == '@').get,
  packetLefts = points.filter(_.charAt == '[').toSet)

val part2 = moves.foldLeft(start)((state, instruction) => state.move(instruction)).packetLefts.toList.map(_.gps).sum
