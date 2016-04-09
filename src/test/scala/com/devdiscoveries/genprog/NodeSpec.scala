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

  "A randomly generated tree" should {
    import Operation._
    val tree = Node.generateRandomTree(params = List(Param(0)),
      operations = List(add[Int], subtract[Int], multiply[Int]),
      maxDepth = 10,
      valueGenerator = () => Random.nextInt)
    "have a depth >= 1" in {
      Node.depth(tree) must be >= 1
    }
    "never be deeper than the maxDepth" in {
      Node.depth(tree) must be <= 10
    }
  }

  "The add operation" should {
    import Operation._
    "add two numbers" in {
      val op = add[Int]
      op(Const(2), Const(5)).compute mustEqual 7
    }
  }

  "The subtract operation" should {
    import Operation._
    "subtract two numbers" in {
      val op = subtract[Int]
      op(Const(2), Const(5)).compute mustEqual -3
    }
  }

  "The multiply operation" should {
    import Operation._
    "multiply two numbers" in {
      val op = multiply[Int]
      op(Const(2), Const(5)).compute mustEqual 10
    }
  }

}