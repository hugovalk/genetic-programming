package com.devdiscoveries.genprog

import scala.util.Random
import scala.math.Numeric

trait GeneticAlgorithm[Individual] {

  def mutate(individual: Individual): Individual

  def crossOver(parent1: Individual, parent2: Individual): Individual

  def evolve

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
}

case class Param[A](name: String) extends Node[A] {
  override def compute(params: Map[String, A]): A =
    params.find(_._1 == name) match {
      case Some(p) => p._2
      case None => throw new IllegalArgumentException("This parameter is not in the list.")
    }
}

case class Const[A](value: A) extends Node[A] {
  override def compute(params: Map[String, A]) = value
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
}

case class Operation[A](nodes: (Node[A], Node[A]), f: (A, A) => A)
    extends Node[A] {
  override def compute(params: Map[String, A]) = f(nodes._1.compute(params), nodes._2.compute(params))
}

trait GeneticProgrammingInterpreter extends GeneticAlgorithm[Node[Int]] {

}

