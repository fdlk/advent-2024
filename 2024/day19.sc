import common.{Memo, loadPackets}

import scala.collection.mutable

val input = loadPackets(List("day19.txt"))
val towels = input.head.split(",").map(_.trim)
val patterns = input.drop(2)

case object Memos {
  def isPossibleInternal(pattern: String): Boolean = pattern.isEmpty ||
      towels.filter(pattern.startsWith)
        .map(_.length)
        .map(pattern.substring)
        .exists(isPossible)

  def countPossibleInternal(pattern: String): Long =
    if pattern.isEmpty then 1L
    else towels.filter(pattern.startsWith)
      .map(_.length)
      .map(pattern.substring)
      .map(countPossible)
      .sum

  val isPossible = Memo(isPossibleInternal)
  val countPossible = Memo(countPossibleInternal)
}

val part1 = patterns.count(Memos.isPossible)
val part2 = patterns.map(Memos.countPossible).sum