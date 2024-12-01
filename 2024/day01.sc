import common.loadPackets

val input = loadPackets(List("day01.txt")).map(_.split("\\s+").map(_.toInt))

val left = input.map(_(0))
val right = input.map(_(1))
val sortedPairs = left.sorted.zip(right.sorted)
val diffs = sortedPairs.map({case (a, b) => Math.abs(a - b)})
val part1 = diffs.sum

def similarityScore(a: Int): Int = a * right.count(_ == a)
val part2 = left.map(similarityScore).sum