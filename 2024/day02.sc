import common.loadPackets

val input = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").map(_.toInt))

def increasing(list: Seq[Int]): Boolean =
  list.zip(list.tail).forall({case (a, b) => a < b})

def decreasing(list: Seq[Int]): Boolean =
  list.zip(list.tail).forall({case (a, b) => a > b})

def diffsSafe(list: Seq[Int]): Boolean =
  list.zip(list.tail).map({case (a, b) => (a - b).abs})
    .forall((1 to 3).contains)

def safe(list: Seq[Int]): Boolean =
  (increasing(list) || decreasing(list)) && diffsSafe(list)

val part1 = input.count(safe)

def dropIndex(list: Seq[Int], index: Int): Seq[Int] =
  list.zipWithIndex
    .filter({case (e, i) => i != index})
    .map(_._1)

def safeWithIndexDropped(list: Seq[Int]): Boolean =
  list.indices.exists(index => safe(dropIndex(list, index)))

val part2 = input.count(safeWithIndexDropped)