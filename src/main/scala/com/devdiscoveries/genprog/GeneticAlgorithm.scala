package com.devdiscoveries.genprog

import scala.util.Random

trait GeneticAlgorithm[Individual] {

  def evolve

}

object Node {
  def generateRandomTree[A](params: Seq[Param[A]],
                            operations: Seq[(Node[A], Node[A]) => Operation[A]],
                            probParams: Double = 0.1,
                            probConst: Double = 0.4,
                            probOperation: Double = 0.5,
                            valueGenerator: () => A): Node[A] = {
    require(probParams + probConst + probOperation == 1.0)
    val random = Random.nextDouble()
    if (random < probParams)
      params(Random.nextInt(params.size))
    else if (random < probParams + probConst)
      Const(valueGenerator())
    else {
      val n1 = generateRandomTree(params, operations, probParams, probConst, probOperation, valueGenerator)
      val n2 = generateRandomTree(params, operations, probParams, probConst, probOperation, valueGenerator)
      operations(Random.nextInt(operations.size))(n1, n2)
    }
  }
}

sealed trait Node[A] {
  def compute: A
}

case class Param[A](var value: A) extends Node[A] {
  override def compute = value
}

case class Const[A](value: A) extends Node[A] {
  override def compute = value
}

case class Operation[A](nodes: (Node[A], Node[A]), f: (A, A) => A)
    extends Node[A] {
  override def compute = f(nodes._1.compute, nodes._2.compute)
}

trait GeneticProgrammingInterpreter extends GeneticAlgorithm[Node[Int]] {

}

