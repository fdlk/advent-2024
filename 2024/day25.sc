import common.loadPackets

val (lockPatterns, keyPatterns) = loadPackets(List("day25.txt")).grouped(8).toList
  .map(_.dropRight(1))
  .partition(_.head.contains("#"))

val rows = lockPatterns.head.indices
val cols = lockPatterns.head.head.indices

val keys = keyPatterns.map(key => cols.map(colIndex => 5 - key.lastIndexWhere(_.charAt(colIndex) == '.')))
val locks = lockPatterns.map(key => cols.map(colIndex => key.lastIndexWhere(_.charAt(colIndex) == '#')))

def fits(lock: Seq[Int], key: Seq[Int]): Boolean = (lock zip key).map(_ + _).forall(_ <= 5)

val part1 = locks.map(lock => keys.count(key => fits(lock, key))).sum