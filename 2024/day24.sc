import common.loadPackets
import reactive.Signal

val input = loadPackets(List("day24.txt"))
val (initialInput, wiresInput) = input.splitAt(input.indexWhere(_.isBlank))

type Signals = Map[String, Signal[Option[Long]]]

val signals: Signals = input.filterNot(_.isBlank).map({
  case s"${wire}: ${value}" => wire -> Signal(Some(value.toLong))
  case s"${foo} -> ${out}" => out -> Signal(None)
}).toMap

wiresInput.tail.map(_.trim).foreach({
  case s"${in1} ${op} ${in2} -> ${out}" => signals(out).update({
    signals(in1)().flatMap(a => signals(in2)().map(b => op match {
      case "AND" => a & b
      case "OR" => a | b
      case "XOR" => a ^ b
    }))
  })
})

val part1 = signals.filter(_._1.startsWith("z")).map({
  case (s"z${index}", signal) => (1L << index.toLong) * signal().get
}).sum