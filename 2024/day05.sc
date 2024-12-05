import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day05.txt"))
val orderings = input.takeWhile(_.nonEmpty).map({
  case s"${a}|${b}" => (a.toInt, b.toInt)
})
def isObserved(ordering: (Int, Int), list: IndexedSeq[Int]): Boolean = {
  val indices = ordering.toList.map(list.indexOf(_))
  indices.contains(-1) || indices == indices.sorted
}
val (good, bad) = input.drop(orderings.size + 1).map(_.split(",").map(_.toInt).toIndexedSeq)
  .partition(list => orderings.forall(isObserved(_, list)))

val part1 = good.map(list => list(list.length / 2)).sum

def findMinimum(elements: Seq[Int]): Int =
  elements.find(e => elements.filter(_ != e)
    .forall(other => !orderings.contains((other, e)))).get

@tailrec
def sort(elements: Seq[Int], sorted: List[Int] = Nil): List[Int] =
  if elements.isEmpty then sorted else {
    val min = findMinimum(elements)
    sort(elements.filter(_ != min), sorted ::: List(min))
  }

val part2 = bad.map(sort(_, Nil)).map(list => list(list.length / 2)).sum