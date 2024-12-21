import common.{Memo, loadPackets}

import scala.annotation.tailrec

case class Pos(row: Int, col: Int):
  def move(d: Char) = d match {
    case '^' => copy(row = row - 1)
    case '>' => copy(col = col + 1)
    case 'v' => copy(row = row + 1)
    case '<' => copy(col = col - 1)
  }

case class Keypad(keypad: IndexedSeq[String]):
  def find(char: Char): Pos =
    val row = keypad.indexWhere(_.contains(char))
    val col = keypad(row).indexOf(char)
    Pos(row, col)

  def contains(pos: Pos): Boolean =
    keypad.indices.contains(pos.row) &&
      keypad.head.indices.contains(pos.col) &&
      charAt(pos) != ' '

  def charAt(pos: Pos): Char = keypad(pos.row)(pos.col)

  def moves(from: Pos, to: Char): Seq[Char] =
    val Pos(fromRow, fromCol) = from
    val Pos(toRow, toCol) = find(to)
    val numDown = toRow - fromRow
    val numRight = toCol - fromCol
    List.fill(numDown)('v') ::: List.fill(-numDown)('^') ::: List.fill(numRight)('>') ::: List.fill(-numRight)('<')

  @tailrec
  final def canType(pos: Pos, code: Seq[Char]): Boolean =
    if code.isEmpty then true
    else
      val moved = pos.move(code.head)
      contains(moved) && canType(moved, code.tail)

  def codes(from: Char, to: Char): Seq[String] =
    val fromPos = find(from)
    moves(fromPos, to).permutations.distinct
      .filter(code => canType(fromPos, code))
      .map(_.mkString).toSeq

val numeric = Keypad(Vector("789", "456", "123", " 0A"))
val directional = Keypad(Vector(" ^A", "<v>"))

case object Memos:
  def humanTypeCost(code: String): Long =
    s"A${code}A".sliding(2)
      .map(pair => directional.codes(pair.head, pair(1)).map(_.length + 1).min)
      .sum

  def robotTypeCost(code: String, n: Int): Long =
    if n == 1 then humanTypeCost(code)
    else s"A${code}A".sliding(2)
      .map(pair => directional.codes(pair.head, pair(1))
        .map(c => costOnNDirectionalKeypads(c, n - 1)).min)
      .sum

  val costOnNDirectionalKeypads: Memo[(String, Int), Long] = Memo(robotTypeCost.tupled)

  def costOnNumericKeypad(code: String, numRobots: Int): Long =
    s"A$code".sliding(2)
      .map(pair => numeric.codes(pair.head, pair(1)).map(c => costOnNDirectionalKeypads(c, numRobots)).min)
      .sum

  def complexity(code: String, numRobots: Int): Long = code.dropRight(1).toInt * costOnNumericKeypad(code, numRobots)

val codes = loadPackets(List("day21.txt"))
val part1 = codes.map(c => Memos.complexity(c, 2)).sum
val part2 = codes.map(c => Memos.complexity(c, 25)).sum