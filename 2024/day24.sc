import common.loadPackets
import reactive.Signal

val input = loadPackets(List("day24.txt"))
val (initialInput, wiresInput) = input.splitAt(input.indexWhere(_.isBlank))

type Signals = Map[String, Signal[Option[Boolean]]]

val signals: Signals = input.filterNot(_.isBlank).map({
  case s"${wire}: ${value}" => wire -> Signal(Some(value == "1"))
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
val z: Signal[Long] = Signal({
  signals.filter(_._1.startsWith("z")).map({
    case (s"z${index}", signal) => (1L << index.toLong) * (if signal().get then 1 else 0)
  }).sum
})

val part1 = z()

def tryAdd(x: Long, y: Long): Long = {
  signals.filter(_._1.startsWith("x")).foreach(_._2.update(Some(false)))
  signals.filter(_._1.startsWith("y")).foreach(_._2.update(Some(false)))
  x.toBinaryString.zipWithIndex.foreach((digit, index) => signals("x%02d".format(index)).update({Some(digit == '1')}))
  y.toBinaryString.zipWithIndex.foreach((digit, index) => signals("y%02d".format(index)).update({Some(digit == '1')}))
  z()
}

tryAdd(0, 0)

