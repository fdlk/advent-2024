import common.loadPackets

val input = loadPackets(List("day01.txt")).map(_.split("\\s+").map(_.toInt))

val List(left, right) = input.transpose

val part1 = left.sorted.zip(right.sorted).map({ case (a, b) => (a - b).abs }).sum
val part2 = left.map(a => a * right.count(_ == a)).sum