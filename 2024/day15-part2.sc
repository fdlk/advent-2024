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

case class Point(x: Int, y: Int):
  def plus(d: Point, steps: Int = 1): Point = Point(x = x + d.x * steps, y = y + d.y * steps)
  def charAt: Char = mapList(y).charAt(x)
  def move(instruction: Char) = instruction match {
    case '^' => copy(y = y - 1)
    case '>' => copy(x = x + 1)
    case 'v' => copy(y = y + 1)
    case '<' => copy(x = x - 1)
  }
  def gps: Int = 100 * y + x

case class State(robot: Point, packetLefts: Set[Point]):
  val packetRights: Set[Point] = packetLefts.map(_.move('>'))
  val packets: Set[Point] = packetLefts ++ packetRights
  def move(instruction: Char): State = robot.move(instruction) match {
    case moved if moved.charAt == '#' => this
    case moved if !packets.contains(moved) => copy(robot = moved)
    case moved => push(packetLeft(moved), instruction).map(_.move(instruction)).getOrElse(this)
  }
  def packetLeft(packet: Point) = if packetRights.contains(packet) then packet.move('<') else packet
  def push(left: Point, instruction: Char): Option[State] = {
    val right = left.move('>')
    val targets: Set[Point] = (instruction match {
      case '^' | 'v' => Set(left, right)
      case '>' => Set(right)
      case '<' => Set(left)
    }).map(_.move(instruction))
    if targets.exists(_.charAt == '#') then None
    else targets.map(packetLeft).intersect(packetLefts)
        .foldLeft[Option[State]](Some(this))((state, blocking) => state.flatMap(_.push(blocking, instruction)))
        .map(pushedState => pushedState.copy(packetLefts = pushedState.packetLefts - left + left.move(instruction)))
  }

val points = for
  x <- mapList.head.indices
  y <- mapList.indices
yield Point(x, y)
val start = State(robot = points.find(_.charAt == '@').get,
                  packetLefts = points.filter(_.charAt == '[').toSet)
val part2 = moves.foldLeft(start)((state, instruction) => state.move(instruction)).packetLefts.toList.map(_.gps).sum
