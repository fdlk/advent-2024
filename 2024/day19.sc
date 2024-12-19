import common.loadPackets
import scala.collection.mutable

val input = loadPackets(List("day19.txt"))
val towels = input.head.split(",").map(_.trim)
val patterns = input.drop(2)

val possiblePatterns: mutable.Map[String, Boolean] = mutable.Map();

def isPossible(pattern: String): Boolean =
  if pattern.isEmpty
  then true
  else possiblePatterns.getOrElse(pattern, {
    val possible = towels.filter(pattern.startsWith)
      .map(_.length)
      .exists(n => isPossible(pattern.substring(n)))
    possiblePatterns.put(pattern, possible)
    possible
  })

val part1 = patterns.count(isPossible)