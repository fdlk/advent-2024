import common.{loadPackets, time}

val input = loadPackets(List("day22.txt")).map(_.toLong)

def mix(value: Long, number: Long): Long = value ^ number
def prune(number: Long): Long = number % 16777216L
def evolve(number: Long): Long = {
  val step1 = prune(mix(number * 64L, number))
  val step2 = prune(mix(step1 / 32L, step1))
  val step3 = prune(mix(step2 * 2048L, step2))
  step3
}

case class Buyer(number: Long):
  val numbers: List[Long] = LazyList.iterate(number)(evolve).take(2001).toList
  val prices: List[Int] = numbers.map(_ % 10).map(_.toInt)
  val diffs: List[Int] = prices.sliding(2).map(pair => pair.last - pair.head).toList
  val slidingDiffs: List[List[Int]] = diffs.sliding(4).map(_.toList).toList
  val priceForDiffs: Map[List[Int], Int] =
    slidingDiffs.zip(prices.drop(4)).foldLeft[Map[List[Int], Int]](Map())((a, b) => if a.contains(b._1) then a else a.updated(b._1, b._2))

val buyers = input.map(Buyer.apply)
val part1 = buyers.map(_.numbers.last).sum

val allDiffs = buyers.flatMap(_.slidingDiffs.toVector).distinct
def totalPriceForDiff(trigger: List[Int]) =
  buyers.flatMap(_.priceForDiffs.get(trigger)).sum

val part2 = allDiffs.map(totalPriceForDiff).max
