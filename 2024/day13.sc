import common.loadPackets

val input = loadPackets(List("day13-test.txt"))

case class Point(x: Int, y: Int):
  def plus(d: Point): Point = copy(x = x + d.x, y = y + d.y)

case class Machine(buttonA: Point, buttonB: Point, prize: Point)

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

