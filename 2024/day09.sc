import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day09.txt")).head
  .map(_.toString.toInt)

type Disk = IndexedSeq[Option[Int]]

val blocks: Disk = input.zipWithIndex
  .flatMap((size, index) => List.fill(size)(if index % 2 == 0 then Some(index / 2) else None))

def trimRight(disk: Disk): Disk =
  disk.dropRight(disk.size - disk.lastIndexWhere(_.isDefined) - 1)

@tailrec
def compress(disk: Disk, index: Int): Disk =
  if index == disk.size then disk
  else disk(index) match {
    case Some(id) => compress(disk, index + 1)
    case None => compress(trimRight(disk.updated(index, disk.last).dropRight(1)), index + 1)
  }

def checksum(disk: Disk): Long = disk.zipWithIndex
  .map({
    case (Some(id), index) => id * index.toLong
    case _ => 0
  }).sum

val part1 = checksum(compress(blocks, 0))

def compress2(disk: Disk, id: Int): Disk =
  if id < 0 then disk
  else {
    val size = disk.count(_.contains(id))
    val targetStart = disk.indices
      .find(i => (0 until size).map(_ + i).forall(index => index < disk.size && disk(index).isEmpty))
    val target = targetStart.map(start => start until start + size)
    if target.isEmpty || targetStart.get > disk.indexOf(Some(id))
    then compress2(disk, id - 1)
    else compress2(disk.zipWithIndex.map({
      case (Some(blockId), _) if blockId == id => None
      case (_, index) if target.get.contains(index) => Some(id)
      case (block, _) => block
    }), id - 1)
  }

val lastId = blocks.flatten.max
val part2 = checksum(compress2(blocks, lastId))