import common.{Memo, loadPackets}

import scala.annotation.tailrec
import scala.collection.mutable

val input = loadPackets(List("day11.txt")).head.split("""\s+""").map(_.toLong).toList

type Stone = Long
type Frequencies = Map[Stone, Long]

def blink(stone: Stone): List[Stone] = stone.toString match {
  case "0" => List(1)
  case digits if digits.length % 2 == 0 =>
    digits.splitAt(digits.length / 2).toList.map(_.toLong)
  case _ => List(2024 * stone)
}

def combineFrequencies(frequencies: Seq[(Stone, Long)]): Frequencies =
  frequencies.groupMap(_._1)(_._2).map((stone, frequencies) => (stone, frequencies.sum))

@tailrec
def blinkN(stones: Frequencies, times: Int): Frequencies =
  if times == 0 then stones
  else
    val blinked = stones.toSeq.flatMap((stone, number) => blink(stone).map(s => s -> number))
    blinkN(combineFrequencies(blinked), times - 1)

val blink5 = Memo((stone: Stone) => blinkN(Map(stone -> 1L), 5))

def next(stones: Frequencies): Frequencies =
  stones.map((stone, freq) => blink5(stone)
      .map((mappedStone, mappedFreq) => (mappedStone, mappedFreq * freq)))
    .reduceLeft((a, b) => combineFrequencies(a.toSeq ++ b))

val start = input.map(stone => stone -> 1L).toMap
val part1 = Iterable.iterate(start, 6)(next).last.values.sum
val part2 = Iterable.iterate(start, 16)(next).last.values.sum