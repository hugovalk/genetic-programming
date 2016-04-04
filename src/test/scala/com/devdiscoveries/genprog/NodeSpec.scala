package com.devdiscoveries.genprog

import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import scala.util.Random

class NodeSpec extends WordSpec with MustMatchers {

  "A constant node" should {
    "evaluate to its value" in {
      Const(3).compute mustEqual 3
      Const("a").compute mustEqual "a"
      Const(1.2).compute mustEqual 1.2
    }
  }

  "A parameter node" should {
    "evaluate to its value" in {
      Param(1).compute mustEqual 1
      Param("b").compute mustEqual "b"
      Param(4.1).compute mustEqual 4.1
    }
  }

  val t = Node.generateRandomTree(params = List(Param(0)),
    operations = List(add(_, _)),
    valueGenerator = () => Random.nextInt)

  def add(node1: Node[Int], node2: Node[Int]): Operation[Int] =
    Operation[Int]((node1, node2), _ + _)

}