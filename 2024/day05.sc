import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day05.txt"))
val orderings = input.takeWhile(_.nonEmpty).map({
  case s"${a}|${b}" => (a.toInt, b.toInt)
})
val lists = input.drop(orderings.size + 1).map(_.split(",").map(_.toInt))

def isObserved(ordering: (Int, Int), list: Array[Int]): Boolean = {
  val indices = ordering.toList.map(list.indexOf(_))
  indices.contains(-1) || indices == indices.sorted
}

val part1 = lists.filter(list => orderings.forall(isObserved(_, list)))
  .map(list => list(list.length / 2))
  .sum

val brokenLists = lists.filterNot(list => orderings.forall(isObserved(_, list)))

def findMinimum(elements: Seq[Int]): Int =
  elements.find(e => elements.filter(_ != e)
    .forall(other => !orderings.contains((other, e)))).get

@tailrec
def sort(elements:Seq[Int], sorted: List[Int] = Nil): List[Int] =
  if elements.isEmpty then sorted else {
    val min = findMinimum(elements)
    sort(elements.filter(_ != min), sorted ::: List(min))
  }

brokenLists.map(list => sort(list.toSeq))
  .map(list => list(list.length / 2))
  .sum