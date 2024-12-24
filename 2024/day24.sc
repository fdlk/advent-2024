import common.loadPackets

val input = loadPackets(List("day24-test1.txt"))
val (initialInput, wiresInput) = input.splitAt(input.indexWhere(_.isBlank))
val initial = initialInput.map({
  case s"${wire}: ${value}" => wire -> value.toInt
}).toMap

case class Connection(in1: String, in2: String, op: String, out: String)

val wires = wiresInput.tail.map(_.trim).map({
  case s"${in1} ${op} ${in2} -> ${out}" => Connection(in1, in2, op, out)
})

