import common.loadPackets

val input = loadPackets(List("day14.txt"))
val (xmax, ymax) = (101, 103)

case class Point(x: Long, y: Long):
  def plus(d: Point, steps: Long = 1): Point =
    Point(x = Math.floorMod(x + d.x * steps, xmax), y = Math.floorMod(y + d.y * steps, ymax))
  def quadrant: Option[Point] = Some(Point((x - xmax / 2).sign, (y - ymax / 2).sign))
    .filter(_.x != 0).filter(_.y != 0)

case class Bot(p: Point, v: Point):
  def posAt(t: Int): Point = p.plus(v, t)

case object Bots:
  def parse(line: String): Bot = line match {
    case s"p=${x},${y} v=${dx},${dy}" =>
      Bot(Point(x.toInt, y.toInt), Point(dx.toInt, dy.toInt))
  }

val bots = input.map(Bots.parse)
val part1 = bots.map(_.posAt(100))
  .flatMap(_.quadrant).groupBy(x => x).values.map(_.size).product

