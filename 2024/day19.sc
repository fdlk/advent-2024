import common.loadPackets
import scala.collection.mutable

val input = loadPackets(List("day19.txt"))
val towels = input.head.split(",").map(_.trim)
val patterns = input.drop(2)

val possible: mutable.Map[String, Boolean] = mutable.Map()

def isPossible(pattern: String): Boolean =
  if pattern.isEmpty
  then true
  else possible.getOrElse(pattern, {
    val result = towels.filter(pattern.startsWith)
      .map(_.length)
      .exists(n => isPossible(pattern.substring(n)))
    possible.put(pattern, result)
    result
  })

val possiblePatterns = patterns.filter(isPossible)
val part1 = possiblePatterns.size

val howManyPatterns: mutable.Map[String, Long] = mutable.Map()

def countPossible(pattern: String): Long =
  if pattern.isEmpty then 1L
  else howManyPatterns.getOrElse(pattern, {
    val result = towels.filter(pattern.startsWith)
      .map(_.length)
      .map(pattern.substring)
      .map(countPossible)
      .sum
    howManyPatterns.put(pattern, result)
    result
  })

val part2 = possiblePatterns.map(countPossible).sum