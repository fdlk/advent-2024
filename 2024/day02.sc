import common.loadPackets

val input = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").map(_.toInt).toSeq)

extension (list: Seq[Int])
  def pairs: Seq[(Int, Int)] = list.zip(list.tail)
  def increasing: Boolean = pairs.forall(_ < _)
  def decreasing: Boolean = pairs.forall(_ > _)
  def monotonous: Boolean = increasing || decreasing
  def diffs: Seq[Int] = pairs.map(_ - _).map(_.abs)
  def gradual: Boolean = diffs.forall((1 to 3).contains)
  def safe: Boolean = monotonous && gradual
  def safeWithIndexDropped: Boolean =
    list.indices.exists(i => list.patch(i, Nil, 1).safe)

val part1 = input.count(_.safe)
val part2 = input.count(_.safeWithIndexDropped)