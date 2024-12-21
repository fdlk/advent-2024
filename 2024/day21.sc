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
  def find(char: Char): Pos = {
    val row = keypad.indexWhere(_.contains(char))
    val col = keypad(row).indexOf(char)
    Pos(row, col)
  }
  def contains(pos: Pos): Boolean =
    keypad.indices.contains(pos.row) &&
      keypad.head.indices.contains(pos.col) &&
      charAt(pos) != ' '
  def charAt(pos: Pos): Char = keypad(pos.row)(pos.col)
  def moves(from: Char, to: Char): Seq[Char] = {
    val Pos(fromRow, fromCol) = find(from)
    val Pos(toRow, toCol) = find(to)
    val vertical: Seq[Char] = toRow - fromRow match {
      case num if num >= 0 => List.fill(num)('v')
      case num => List.fill(-num)('^')
    }
    val horizontal: Seq[Char] = toCol - fromCol match {
      case num if num >= 0 => List.fill(num)('>')
      case num => List.fill(-num)('<')
    }
    vertical ++ horizontal
  }
  def move(pos: Pos, d: Char): Option[Pos] = Some(pos.move(d)).filter(contains)
  @tailrec
  final def canType(pos: Pos, code: Seq[Char]): Boolean =
    if code.isEmpty then true
    else
      val moved = move(pos, code.head)
      moved.isDefined && canType(moved.get, code.tail)
  def codesInternal(from: Char, to: Char): Seq[String] =
    moves(from, to).permutations.distinct.filter(code => canType(find(from), code)).map(_.mkString).toSeq
  val codes: Memo[(Char, Char), Seq[String]] = Memo((from, to) => codesInternal(from, to))

val numeric = Keypad(Vector("789", "456", "123", " 0A"))
val directional = Keypad(Vector(" ^A","<v>"))

// hoe duur is het om deze code in te tikken met een A erachter als je in A staat
def costOnDirectionalKeypad(code: String) =
  s"A${code}A".sliding(2)
    .map(pair => directional.codes(pair.head, pair(1)).map(_.length + 1).min)
    .sum

def costOnTwoDirectionalKeypads(code: String) =
  s"A${code}A".sliding(2)
    .map(pair => directional.codes(pair.head, pair(1)).map(costOnDirectionalKeypad).min)
    .sum

def costOnNumericKeypad(code: String) =
  s"A${code}".sliding(2)
    .map(pair => numeric.codes(pair.head, pair(1)).map(costOnTwoDirectionalKeypads).min)
    .sum

costOnNumericKeypad("029A")

def complexity(code: String): Int = code.dropRight(1).toInt * costOnNumericKeypad(code)

val codes = loadPackets(List("day21.txt"))
val part1 = codes.map(complexity).sum
