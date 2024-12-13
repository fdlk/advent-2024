import common.loadPackets

val input = loadPackets(List("day13.txt"))

case class Point(x: Long, y: Long):
  def plus(d: Point, steps: Long = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

case class Machine(buttonA: Point, buttonB: Point, prize: Point):
  def cost(p: Point) =
    val b = (buttonA.y * p.x - buttonA.x * p.y) / (buttonB.x * buttonA.y - buttonA.x * buttonB.y)
    val a = (p.x - b * buttonB.x) / buttonA.x
    if Point(0, 0).plus(buttonA, a).plus(buttonB, b) == p
    then Some(3 * a + b) else None
  def costPart1 = cost(prize)
  def costPart2 = cost(prize.plus(Point(10000000000000L, 10000000000000L)))

val machines = input.grouped(4).map({
  case List(
  s"Button A: X${dxa}, Y${dya}",
  s"Button B: X${dxb}, Y${dyb}",
  s"Prize: X=${x}, Y=${y}"
  , _) => Machine(
    Point(dxa.toInt, dya.toInt),
    Point(dxb.toInt, dyb.toInt),
    Point(x.toInt, y.toInt)
  )
}).toList

val part1 = machines.flatMap(_.costPart1).sum
val part2 = machines.flatMap(_.costPart2).sum
