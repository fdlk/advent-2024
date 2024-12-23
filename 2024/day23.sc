import common.loadPackets

import scala.annotation.tailrec

val connections: Set[Set[String]] = loadPackets(List("day23.txt")).map(_.split("-").map(_.trim).toSet).toSet
val nodes: List[String] = connections.flatten.toList.sorted

val part1 = nodes.combinations(3)
  .filter(_.exists(_.startsWith("t")))
  .count(group => group.combinations(2).map(_.toSet).forall(connections.contains))

@tailrec
def groups(group: List[String], open: List[String], soFar: List[List[String]]): List[List[String]] =
  if group.isEmpty then
    if open.isEmpty then soFar
    else groups(List(open.head), open.tail, soFar)
  else
    val found = open.find(candidate => group.map(member => Set(member, candidate)).forall(connections.contains))
    if found.isEmpty
    then groups(Nil, open, group :: soFar)
    else groups(group ++ found, open.filterNot(found.contains), soFar)

val part2 = groups(Nil, nodes, Nil).maxBy(_.size).mkString(",")