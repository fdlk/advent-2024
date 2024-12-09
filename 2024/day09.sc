import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day09.txt")).head.map(_.toString.toInt)

type Disk = IndexedSeq[Option[Int]]

val blocks: Disk = input.zipWithIndex
  .flatMap((size, index) => List.fill(size)(if index % 2 == 0 then Some(index / 2) else None))

def trimRight(disk: Disk): Disk = disk.dropRight(disk.size - disk.lastIndexWhere(_.isDefined) - 1)

def checksum(disk: Disk): Long = disk.zipWithIndex
  .map({
    case (Some(id), index) => id * index.toLong
    case _ => 0
  }).sum

@tailrec
def compress(disk: Disk, index: Int = 0): Disk =
  if index == disk.size then disk
  else disk(index) match {
    case Some(id) => compress(disk, index + 1)
    case None => compress(trimRight(disk.updated(index, disk.last).dropRight(1)), index + 1)
  }

val part1 = checksum(compress(blocks))

@tailrec
def compress2(disk: Disk, id: Int): Disk =
  if id < 0 then disk
  else {
    val source: Range = disk.indexOf(Some(id)) to disk.lastIndexOf(Some(id))
    val compressed: Option[Disk] = disk.indices
      .filter(_ < source.start)
      .map(start => source.indices.map(_ + start))
      .find(_.forall(disk(_).isEmpty))
      .map(target => disk.zipWithIndex.map({
        case (Some(blockId), _) if blockId == id => None
        case (_, index) if target.contains(index) => Some(id)
        case (block, _) => block
      }))
    compress2(compressed.getOrElse(disk), id - 1)
  }

val part2 = checksum(compress2(blocks, blocks.flatten.max))