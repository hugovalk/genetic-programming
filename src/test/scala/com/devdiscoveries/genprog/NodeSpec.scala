package com.devdiscoveries.genprog

import com.devdiscoveries.genprog.Operation._
import org.scalatest.WordSpec
import org.scalatest.MustMatchers
import scala.util.{Failure, Success, Try, Random}

class NodeSpec extends WordSpec with MustMatchers {

  "A constant node" should {
    "evaluate to its value" in {
      Const(3).compute(Map()) mustEqual 3
      Const("a").compute(Map()) mustEqual "a"
      Const(1.2).compute(Map()) mustEqual 1.2
    }
  }

  "A parameter node" should {
    "evaluate to its value" in {
      Param[Int]("name").compute(Map("name" -> 1)) mustEqual 1
      Param[String]("name").compute(Map("name" -> "b")) mustEqual "b"
    }
    "throw an exception when the input does not contain the parameter" in {
      Try {
        Param[Double]("name").compute(Map("name2" -> 4.1))
      } match {
        case Success(s) => fail()
        case Failure(t) => t.isInstanceOf[IllegalArgumentException]
      }
    }
  }

  "A randomly generated tree" should {
    import Operation._
    val tree = Node.generateRandomTree[Int](params = List(Param("name")),
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
      op(Const(2), Const(5)).compute(Map()) mustEqual 7
    }
  }

  "The subtract operation" should {
    import Operation._
    "subtract two numbers" in {
      val op = subtract[Int]
      op(Const(2), Const(5)).compute(Map()) mustEqual -3
    }
  }

  "The multiply operation" should {
    import Operation._
    "multiply two numbers" in {
      val op = multiply[Int]
      op(Const(2), Const(5)).compute(Map()) mustEqual 10
    }
  }

  "The length" should {
    "be 1 for a constant" in {
      Const(4).length mustEqual 1
    }
    "be 1 for a parameter" in {
      Param("test").length mustEqual 1
    }
    "be 3 for an operation" in {
      multiply[Int].apply(Const(1), Const(1)).length mustEqual 3
    }
    "be 5 for a nested operation" in {
      multiply[Int].apply(add[Int].apply(Const(1), Const(1)), Const(1)).length mustEqual 5
    }
  }

  "The childAtIndex method" should {
    "return the root at index 0" in {
      Const(1).childAtIndex(0) mustEqual Const(1)
    }
    "throw an IndexOutOfBoundsException for index > 0 in constants" in {
      Try{Const(1).childAtIndex(1)} match {
        case Success(_) => fail()
        case Failure(t) => t.asInstanceOf[IndexOutOfBoundsException]
      }
    }
    "return left child of an operation" in {
      add[Int].apply(Const(1), Const(2)).childAtIndex(1) mustEqual Const(1)
    }
    "return right child of an operation" in {
      add[Int].apply(Const(1), Const(2)).childAtIndex(2) mustEqual Const(2)
    }
  }





}