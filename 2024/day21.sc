import common.{Memo, loadPackets}

import scala.annotation.tailrec

case class Pos(row: Int, col: Int):
  def move(d: Char) = d match {
    case '^' => copy(row = row - 1)
    case '>' => copy(col = col + 1)
    case 'v' => copy(row = row + 1)
    case '<' => copy(col = col - 1)
  }
  def minus(other: Pos): Pos = copy(row = row - other.row, col = col - other.col)

case class Keypad(keypad: IndexedSeq[String]):
  def find(char: Char): Pos =
    val row = keypad.indexWhere(_.contains(char))
    val col = keypad(row).indexOf(char)
    Pos(row, col)

  def contains(pos: Pos): Boolean =
    keypad.indices.contains(pos.row) &&
      keypad.head.indices.contains(pos.col) &&
      keypad(pos.row)(pos.col) != ' '

  def moves(from: Pos, to: Pos): String =
    val Pos(numDown, numRight) = to.minus(from)
    (List.fill(numDown)('v') ::: List.fill(-numDown)('^') :::
      List.fill(numRight)('>') ::: List.fill(-numRight)('<')).mkString

  @tailrec
  final def canType(pos: Pos, code: String): Boolean =
    if code.isEmpty then true
    else
      val moved = pos.move(code.head)
      contains(moved) && canType(moved, code.tail)

  def codes(from: Char, to: Char): Seq[String] =
    val fromPos = find(from)
    moves(fromPos, find(to)).permutations.distinct
      .filter(canType(fromPos, _))
      .map(_ + 'A').toSeq

val numeric = Keypad(Vector("789", "456", "123", " 0A"))
val directional = Keypad(Vector(" ^A", "<v>"))

case object Robots:
  def typeCost(keypad: Keypad, code: String, numRobots: Int): Long =
    if numRobots == 0
    then code.length
    else code.prepended('A').sliding(2)
      .map(pair => keypad.codes(pair.head, pair.last))
      .map(_.map(typeCostMemo(directional, _, numRobots - 1)).min)
      .sum
  val typeCostMemo: Memo[(Keypad, String, Int), Long] = Memo(typeCost.tupled)

def complexity(code: String, numRobots: Int): Long = code.dropRight(1).toInt *
  Robots.typeCostMemo(numeric, code, numRobots)

val codes = loadPackets(List("day21.txt"))
val part1 = codes.map(c => complexity(c, 3)).sum
val part2 = codes.map(c => complexity(c, 26)).sum