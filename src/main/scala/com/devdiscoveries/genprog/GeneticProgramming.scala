package com.devdiscoveries.genprog

import scala.math.Numeric
import scala.util.Random
import scalaz._
import Kleisli._

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

trait GeneticProgramming[A] extends SingleThreadedGeneticAlgorithm[Node[A]] {

  override def generateRandomIndividual: Node[A] = generateTree

  def generateTree: Node[A]

  override def mutate: GAOperation[Node[A], Node[A]] =
    kleisli { individual =>
      individual.replaceAt(Random.nextInt(individual.length), generateTree)
    }

  override def crossOver: GAOperation[(Node[A], Node[A]), Node[A]] =
    kleisli { parents =>
      val replacement = parents._2.childAtIndex(Random.nextInt(parents._2.length))
      parents._1.replaceAt(Random.nextInt(parents._1.length), replacement)
    }
}