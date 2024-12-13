import common.loadPackets

val input = loadPackets(List("day13.txt"))

case class Point(x: Int, y: Int):
  def plus(d: Point, steps: Int = 1): Point = copy(x = x + d.x * steps, y = y + d.y * steps)

case class Machine(buttonA: Point, buttonB: Point, prize: Point):
  def cost =
    (for a <- 0 to 100
         b <- 0 to 100
         if Point(0, 0).plus(buttonA, a).plus(buttonB, b) == prize
    yield 3 * a + b).headOption

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

val part1 = machines.flatMap(_.cost).sum

