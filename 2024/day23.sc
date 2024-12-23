import common.loadPackets

import scala.annotation.tailrec

val connections: Set[Set[String]] = loadPackets(List("day23.txt")).map(_.split("-").map(_.trim).toSet).toSet
val nodes: List[String] = connections.flatten.toList.sorted

val part1 = nodes.combinations(3)
  .filter(_.exists(_.startsWith("t")))
  .count(group => group.combinations(2).map(_.toSet).forall(connections.contains))



