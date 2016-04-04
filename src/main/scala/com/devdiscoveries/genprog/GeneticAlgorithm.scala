package com.devdiscoveries.genprog

trait GeneticAlgorithm[Individual] {

  def evolve

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

case class Operation2[A](nodes: (Node[A], Node[A]), f: (A, A) => A)
    extends Node[A] {
  override def compute = f(nodes._1.compute, nodes._2.compute)
}

case class Operation3[A](nodes: (Node[A], Node[A], Node[A]), f: (A, A, A) => A)
    extends Node[A] {
  override def compute = f(nodes._1.compute, nodes._2.compute, nodes._3.compute)
}

trait GeneticProgrammingInterpreter extends GeneticAlgorithm[Node[Int]] {

}

