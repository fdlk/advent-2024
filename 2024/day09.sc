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

val part1 = compress(blocks, 0).map(_.get.toLong).zipWithIndex.map(_ * _).sum