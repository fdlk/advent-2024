import common.{Memo, loadPackets}

import scala.annotation.tailrec
import scala.collection.mutable

val input = loadPackets(List("day11.txt"))
  .head.split("""\s+""").map(_.toLong).toList

type Stone = Long

def blink(stone: Stone): Map[Stone, Long] = stone.toString match {
    case "0" => Map(1L -> 1L)
    case digits if digits.length % 2 == 0 =>
      val (left, right) = digits.splitAt(digits.length / 2)
      if left == right then Map(left.toLong -> 2)
      else Map(left.toLong -> 1, right.toLong -> 1)
    case _ => Map(2024 * stone -> 1)
  }

def combineFrequencies(map1: Map[Stone, Long], map2: Map[Stone, Long]): Map[Stone, Long] =
  (map1.toSeq ++ map2).groupMap(_._1)(_._2)
    .map((stone, frequencies) => (stone, frequencies.sum))

def blinkN(stones: Map[Stone, Long], times: Int): Map[Stone, Long] =
  if times == 0 then stones
  else stones.map((stone, number) => blinkN(blink(stone).map((s, n) => (s, n * number)), times - 1))
      .reduceLeft(combineFrequencies)

val explode25 = Memo((stone: Stone) => blinkN(Map(stone -> 1L), 25))

def next(stones: Map[Stone, Long]): Map[Stone, Long] =
  stones.map((stone, freq) => explode25(stone)
      .map((mappedStone, mappedFreq) => (mappedStone, mappedFreq * freq)))
    .reduceLeft(combineFrequencies)

val start = input.map(stone => stone -> 1L).toMap
val part1 = next(start).values.sum
val part2 = Iterable.iterate(start, 4)(next).last.values.sum