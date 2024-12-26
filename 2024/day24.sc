import common.loadPackets
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

import scala.annotation.tailrec
import scala.util.Random

val input = loadPackets(List("day24.txt"))
val (initialInput, wiresInput) = input.splitAt(input.indexWhere(_.isBlank))

type Signals = Map[String, Boolean]

case class Gate(in1: String, in2: String, op: (Boolean, Boolean) => Boolean, out: String):
  def resolve(signals: Signals): Option[(String, Boolean)] =
    signals.get(in1).flatMap(a => signals.get(in2).map(b => (out, op(a, b))))

  def swap(inputs: Set[String]): Gate =
    copy(out = if inputs.contains(out) then inputs.filterNot(_ == out).head else out)

val initialGates = wiresInput.tail.map(_.trim).map({
  case s"${in1} ${op} ${in2} -> ${out}" => op match {
    case "AND" => Gate(in1, in2, _ & _, out)
    case "OR" => Gate(in1, in2, _ | _, out)
    case "XOR" => Gate(in1, in2, _ ^ _, out)
  }
})

val initialInputs = initialInput.map({
  case s"${in}: ${value}" => (in, value == "1")
}).toMap

val zs = initialGates.map(_.out).filter(_.startsWith("z")).sorted

def toSignals(prefix: Char, value: Long): Signals =
  value.toBinaryString.reverse.padTo(50, '0').zipWithIndex.map((digit, index) => (s"${prefix}%02d".format(index), digit == '1')).toMap

def fromSignals(prefix: Char, signals: Signals): Long = signals.map {
  case (key, true) if key.startsWith(s"${prefix}") => 1L << key.drop(1).toLong
  case _ => 0
}.sum

case class Circuit(gates: List[Gate] = initialGates):
  def swap(swaps: List[List[String]]): Circuit = copy(
    gates = swaps.foldLeft(gates)((soFar, outputs) => soFar.map(_.swap(outputs.toSet)))
  )

  @tailrec
  final def resolveSignals(resolved: Signals = Map()): Signals = {
    val newlyResolved = gates.filterNot(gate => resolved.contains(gate.out)).flatMap(_.resolve(resolved)).toMap
    if newlyResolved.isEmpty
    then resolved
    else resolveSignals(resolved ++ newlyResolved)
  }

  def z(inputs: Signals): Option[Long] = {
    val resolved = resolveSignals(inputs)
    if zs.forall(resolved.contains) then
      Some(fromSignals('z', resolved))
    else None
  }

  def countErrors(x: Long, y: Long): Int = {
    val resolved = resolveSignals(toSignals('x', x) ++ toSignals('y', y))
    z(resolved).map(z => z.toBinaryString.zip((x + y).toBinaryString).count(_ != _)).getOrElse(100)
  }

val initialCircuit = Circuit()
val part1 = initialCircuit.z(initialInputs).get

val maxString = List.fill(45)({
  "1"
}).mkString
val max = java.lang.Long.parseLong(maxString, 2)
val random: Random = Random()
val outputs: List[String] = wiresInput.drop(1).map({
  case s"${in1} ${op} ${in2} -> ${out}" => out.trim
})

val testcases: List[Long] = 0L :: 0L :: 0L :: max :: max :: 0L :: (0 until 20).map(x => random.nextLong(max)).toList

case class Swaps(swaps: List[String]):
  val groups: List[List[String]] = swaps.grouped(2).toList
  val circuit = initialCircuit.swap(groups)
  lazy val score: Int = testcases.grouped(2).map({ case List(x, y) => circuit.countErrors(x, y) }).sum

  def mutate: Swaps = {
    val swapIndex = random.nextInt(swaps.length)
    val candidates = outputs.filterNot(swaps.contains)
    val candidateIndex = random.nextInt(candidates.length)
    copy(swaps.updated(swapIndex, candidates(candidateIndex)))
  }

  def crossingOver(other: Swaps): Swaps = {
    val groupIndex = random.nextInt(groups.length)
    Swaps(groups.updated(groupIndex, other.groups(groupIndex)).flatten)
  }

val swaps = Swaps(Random.shuffle(outputs).take(8).sorted)
swaps.score

case class Population(individuals: List[Swaps]):
  val foo = individuals.par.map(_.score).toList
  val sorted = individuals.sortBy(individual => individual.score)
  val elite = sorted.take(20)
  val survivors = sorted.take(500)
  val crossedOver = survivors.combinations(2).take(400).map({
    case List(a, b) => a.crossingOver(b)
  })
  val mutated = survivors.map(_.mutate)

  def evolve(): Population = Population(elite ++ crossedOver ++ mutated)

val candidate = Swaps(List("qjj", "gjc", "qsb", "z39", "z26", "gvm", "vfq", "ffg"))
val individuals = candidate :: List.fill(199)({
  Swaps(Random.shuffle(outputs).take(8))
})


val pop: Population = Population(individuals)
pop.elite.head.score
pop.evolve().elite.head.score

val evolution = LazyList.iterate(pop)(_.evolve())
  .map(_.elite.head)

val evolved = evolution
  .sliding(3)
  .map(_.toList)
  .find({case List(a, _, b) => b.score == a.score})
  .get.head

evolved.score

val solution: Swaps = evolution.find(_.score == 0).get
val part2 = solution.swaps.sorted.mkString(",")

// val solution: Swaps = Swaps(List(qjj, gjc, qsb, z39, z26, gvm, wmp, z17))
// gjc,gvm,qjj,qsb,wmp,z17,z26,z39