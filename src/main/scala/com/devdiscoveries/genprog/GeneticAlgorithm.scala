package com.devdiscoveries.genprog

import com.devdiscoveries.genprog.Operation._

import scala.util.Random
import scala.math.Numeric

trait GeneticAlgorithm[Individual] {
  /** Absolute number. 0 is perfect fitness */
  type Fitness = Double
  type FitnessFunction = Population => RankedPopulation
  type Population = Seq[Individual]
  type RankedPopulation = Seq[(Individual, Fitness)]

  def mutate(individual: Individual): Individual

  def crossOver(parent1: Individual, parent2: Individual): Individual

  def fitnessFunction: FitnessFunction

  def generateRandomIndividual: Individual

  def evolve(populationSize: Int, numberOfGenerations: Int = 100): Population = {
    val initialPopulation: Population = (1 to numberOfGenerations).map(_ => generateRandomIndividual)
    (1 to numberOfGenerations).foldLeft(initialPopulation) { (x, y) =>
      println(s"Evolving generation $y")
      evolveNewGeneration(x)
    }
  }

  def evolveNewGeneration(population: Population): Population = {
    val rankedPopulation = fitnessFunction(population)
    val selectedParents = truncationParentSelection(rankedPopulation)
    val elite = selectElite(rankedPopulation).map(_._1)
    elite ++ (for {
      i <- elite.size until population.length
    } yield breed(selectedParents))
  }

  def breed(selectedParents: RankedPopulation): Individual = {
    val parent1 = selectedParents(Random.nextInt(selectedParents.size))._1
    val parent2 = selectedParents(Random.nextInt(selectedParents.size))._1
    val child = crossOver(parent1, parent2)
    if (Random.nextDouble() < 0.01)
      mutate(child)
    else
      child
  }

  def selectElite(population: RankedPopulation): RankedPopulation =
    population.sortBy(_._2).take(3)

  def truncationParentSelection(population: RankedPopulation): RankedPopulation =
    population.sortBy(_._2).take(population.length / 3)

}

object Node {
  def generateRandomTree[A](params: Seq[Param[A]],
                            operations: Seq[(Node[A], Node[A]) => Operation[A]],
                            maxDepth: Int = 10,
                            probParams: Double = 0.1,
                            probConst: Double = 0.4,
                            probOperation: Double = 0.5,
                            valueGenerator: () => A): Node[A] = {
    require(probParams + probConst + probOperation == 1.0)

    def createNode(depth: Int): Node[A] = {
      val random = Random.nextDouble()
      if (random < probParams)
        params(Random.nextInt(params.size))
      else if (random < probParams + probConst)
        Const(valueGenerator())
      else if (depth < maxDepth) {
        val n1 = createNode(depth + 1)
        val n2 = createNode(depth + 1)
        operations(Random.nextInt(operations.size))(n1, n2)
      } else
        createNode(depth)
    }
    createNode(1)
  }

  def depth[A](tree: Node[A]): Int = tree match {
    case Operation(nodes, _) =>
      val maxLeft = depth(nodes._1) + 1
      val maxRight = depth(nodes._2) + 1
      if (maxLeft > maxRight) maxLeft else maxRight
    case _ => 1
  }
}

sealed trait Node[A] {
  def compute(params: Map[String, A]): A

  def children: Seq[Node[A]] = Seq()

  def length: Int

  def replaceAt(index: Int, replacement: Node[A]): Node[A] = {
    if (index == 0)
      replacement
    else
      this match {
        case Operation((left, right), f) =>
          val childIndex = index - 1
          if (childIndex < left.length) Operation((left.replaceAt(childIndex, replacement), right), f)
          else Operation((left, right.replaceAt(childIndex - left.length, replacement)), f)
        case _ => throw new IndexOutOfBoundsException(index.toString)
      }
  }

  def childAtIndex(index: Int): Node[A] = {
    if (index == 0)
      this
    else
      this match {
        case Operation((left, right), f) =>
          val childIndex = index - 1
          if (childIndex < left.length) left.childAtIndex(childIndex)
          else right.childAtIndex(childIndex - left.length)
        case _ => throw new IndexOutOfBoundsException(index.toString)
      }
  }
}

case class Param[A](name: String) extends Node[A] {
  override def compute(params: Map[String, A]): A =
    params.find(_._1 == name) match {
      case Some(p) => p._2
      case None    => throw new IllegalArgumentException("This parameter is not in the list.")
    }
  override def length = 1
}

case class Const[A](value: A) extends Node[A] {
  override def compute(params: Map[String, A]) = value
  override def length = 1
}

object Operation {
  def add[N](implicit n: Numeric[N]) = {
    import n._
    (node1: Node[N], node2: Node[N]) => Operation[N]((node1, node2), _ + _)
  }
  def subtract[N](implicit n: Numeric[N]) = {
    import n._
    (node1: Node[N], node2: Node[N]) => Operation[N]((node1, node2), _ - _)
  }
  def multiply[N](implicit n: Numeric[N]) = {
    import n._
    (node1: Node[N], node2: Node[N]) => Operation[N]((node1, node2), _ * _)
  }
  def greaterThan[N](implicit n: Numeric[N]) = {
    import n._
    (node1: Node[N], node2: Node[N]) => Operation[N]((node1, node2), (n1, n2) => if (n1.toDouble() > n2.toDouble()) n1 else n2)
  }
}

case class Operation[A](nodes: (Node[A], Node[A]), f: (A, A) => A)
    extends Node[A] {
  override def compute(params: Map[String, A]) = f(nodes._1.compute(params), nodes._2.compute(params))

  override def children: Seq[Node[A]] = Seq(nodes._1, nodes._2)

  override def length = 1 + nodes._1.length + nodes._2.length
}

trait GeneticProgrammingInterpreter[A] extends GeneticAlgorithm[Node[A]] {

  override def generateRandomIndividual = generateTree

  def generateTree: Node[A]

  override def mutate(root: Node[A]): Node[A] = {
    root.replaceAt(Random.nextInt(root.length), generateTree)
  }

  override def crossOver(parent1: Node[A], parent2: Node[A]): Node[A] = {
    val replacement = parent2.childAtIndex(Random.nextInt(parent2.length))
    parent1.replaceAt(Random.nextInt(parent1.length), replacement)
  }
}

