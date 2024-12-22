import common.loadPackets

val input = loadPackets(List("day22.txt")).map(_.toLong)

def mix(value: Long, number: Long): Long = value ^ number
def prune(number: Long): Long = number % 16777216L
def evolve(number: Long): Long = {
  val step1 = prune(mix(number * 64L, number))
  val step2 = prune(mix(step1 / 32L, step1))
  val step3 = prune(mix(step2 * 2048L, step2))
  step3
}

val part1 = input.map(number => LazyList.iterate(number)(evolve).drop(2000).head).sum