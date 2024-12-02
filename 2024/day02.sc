import common.loadPackets

val input = loadPackets(List("day02.txt"))
  .map(_.split("\\s+").map(_.toInt).toSeq)

extension (list: Seq[Int])
  def pairs: Seq[(Int, Int)] = list.zip(list.tail)
  def increasing: Boolean = pairs.forall(_ < _)
  def decreasing: Boolean = pairs.forall(_ > _)
  def monotonous: Boolean = increasing || decreasing
  def diffsWithinRange(r: Range): Boolean =
    pairs.map(_ - _).map(_.abs).forall(r.contains)
  def safe: Boolean = monotonous && diffsWithinRange(1 to 3)
  def safeWithIndexDropped: Boolean =
    list.indices.exists(i => list.patch(i, Nil, 1).safe)

val part1 = input.count(_.safe)
val part2 = input.count(_.safeWithIndexDropped)