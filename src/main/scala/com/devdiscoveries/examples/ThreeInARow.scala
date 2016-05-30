package com.devdiscoveries.examples

import com.devdiscoveries.genprog.Operation._
import com.devdiscoveries.genprog.{ConcurrentGeneticProgrammingAlgorithn, SimpleGeneticProgrammingAlgorithm, Param, Node}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import scala.util.Random
import scalaz._
import Kleisli._

/**
  * Example game 'Three in a row". This game needs an integer
  * input (between 1 - 9) and is therefore suitable as a genetic
  * programming challenge.
  */
object ThreeInARow extends App {
  //  val winningPlayer = new Game(new ConsolePlayer(X), new ConsolePlayer(O)).play

  val tree =
    Node.generateRandomTree[Int](params = List(Param("0"),
      Param("1"), Param("2"), Param("3"), Param("4"),
      Param("5"), Param("6"), Param("7"), Param("8")),
      operations = List(add[Int], subtract[Int], multiply[Int], greaterThan[Int]),
      valueGenerator = () => Random.nextInt)
  val winningPlayer = new Game(new ConsolePlayer(X), new GPPlayer(O, tree)).play
  println(s"The winner is: $winningPlayer")
}

class SimpleThreeInARowGPAlgorithm extends SimpleGeneticProgrammingAlgorithm[Int] {
  override def generateTree: Node[Int] = Node.generateRandomTree[Int](params = List(Param("0"),
    Param("1"), Param("2"), Param("3"), Param("4"),
    Param("5"), Param("6"), Param("7"), Param("8")),
    operations = List(add[Int], subtract[Int], multiply[Int], greaterThan[Int]),
    maxDepth = 50,
    valueGenerator = () => Random.nextInt)

  override def rankPopulation: RankPopulationFunction =
    kleisli { population =>
      var calculated: Map[Int, Double] = Map()
      val indexedPopulation = population.zipWithIndex

      def loop(curPop: Seq[(Node[Int], Int)]) = {
        if (curPop.size > 1) {
          curPop.tail.foreach { tree =>
            val winner = new Game(new GPPlayer(X, curPop.head._1), new GPPlayer(O, tree._1)).play
            winner match {
              case X =>
                val r = calculated.getOrElse(curPop.head._2, 0.0) + 2.0
                calculated = calculated + (curPop.head._2 -> r)
              case Empty =>
                val r = calculated.getOrElse(curPop.head._2, 0.0) + 1.0
                calculated = calculated + (curPop.head._2 -> r)
                val r2 = calculated.getOrElse(tree._2, 0.0) + 1.0
                calculated = calculated + (tree._2 -> r2)
              case O =>
                val r = calculated.getOrElse(tree._2, 0.0) + 2.0
                calculated = calculated + (tree._2 -> r)
            }
          }
        }
      }
      loop(indexedPopulation)
      indexedPopulation.map(e => (e._1, calculated.getOrElse(e._2, 0.0)))
    }
}

class ConcurrentThreeInARowGPAlgorithm extends ConcurrentGeneticProgrammingAlgorithn[Int] {
  override def generateTree: Node[Int] = Node.generateRandomTree[Int](params = List(Param("0"),
    Param("1"), Param("2"), Param("3"), Param("4"),
    Param("5"), Param("6"), Param("7"), Param("8")),
    operations = List(add[Int], subtract[Int], multiply[Int], greaterThan[Int]),
    maxDepth = 50,
    valueGenerator = () => Random.nextInt)

  override def rankPopulation: RankPopulationFunction =
    kleisli { population =>
      def play(player1: (Node[Int], Int), player2: (Node[Int], Int)): Future[Seq[(Int, Double)]] = Future {
        val winner = new Game(new GPPlayer(X, player1._1), new GPPlayer(O, player2._1)).play
        winner match {
          case X =>
            Seq((player1._2, 2.0))
          case Empty =>
            Seq((player1._2, 1.0), (player2._2, 1.0))
          case O =>
            Seq((player2._2, 2.0))
        }
      }

      def loop2(curPop: Seq[(Node[Int], Int)]): List[Future[Seq[(Int, Double)]]] = {
        if (curPop.size > 1) {
          val scores = Future.sequence(curPop.tail.map { node =>
            play(curPop.head, node)
          }).map(_.flatten)
          scores :: loop2(curPop.tail)
        } else
          List()
      }

      val indexedPopulation = population.zipWithIndex
      val scores = Future.sequence(loop2(indexedPopulation))
      scores.map{scs =>
        val groupedScores = scs.flatten.groupBy(_._1)
        val calculated = groupedScores.map(s => (s._1, s._2.foldLeft(0.0)(_ + _._2)))
        indexedPopulation.map(e => (e._1, calculated.getOrElse(e._2, 0.0)))
      }
    }
}

sealed trait Value

case object X extends Value

case object O extends Value

case object Empty extends Value

trait Player {
  def readMove(board: List[Value]): Int

  def color: Value

  def isHuman: Boolean
}

case class ConsolePlayer(color: Value) extends Player {
  override val isHuman = true

  override def readMove(board: List[Value]) = {
    println(s"Player $color, enter your move (1 - 9):")
    StdIn.readInt()
  }
}

case class GPPlayer(color: Value, tree: Node[Int]) extends Player {
  override val isHuman = false

  override def readMove(board: List[Value]) = {
    val params = board.zipWithIndex.map { e =>
      e._1 match {
        case Empty => (e._2.toString, 0)
        case v =>
          if (v == color)
            (e._2.toString, 1)
          else
            (e._2.toString, -1)
      }
    }.toMap
    Math.abs(tree.compute(params)) % 9 + 1
  }

}

class Game(playerOne: Player, playerTwo: Player) {

  var board: List[Value] = (1 to 9).toList.map(_ => Empty)

  def hasHumanPlayer = playerOne.isHuman ||
    playerTwo.isHuman

  def play: Value = {
    var currentPlayer = if (Random.nextFloat() < 0.5) playerOne else playerTwo
    def loop: Value = {
      printBoard
      if (playMove(currentPlayer)) {
        winner match {
          case Some(v) =>
            printBoard
            v
          case None =>
            currentPlayer = otherPlayer(currentPlayer)
            loop
        }
      } else {
        otherPlayer(currentPlayer).color
      }
    }
    loop
  }

  def otherPlayer(currentPlayer: Player) = {
    if (currentPlayer == playerOne)
      playerTwo
    else
      playerOne
  }

  def playMove(player: Player): Boolean = {
    val move = player.readMove(board)
    if (board(move - 1) != Empty) false
    else {
      board = board.take(move - 1) ++ (player.color :: board.drop(move - 1).tail)
      true
    }
  }

  def rows = for {
    i <- 0 to 8 by 3
  } yield board.slice(i, i + 3)

  def cols = (for {
    i <- 0 to 2
  } yield i to 8 by 3).map(c => c.map(board(_)))

  def diagonals = List(
    List(board(0), board(4), board(8)),
    List(board(2), board(4), board(6)))

  def winner: Option[Value] = {
    def wins(v: Value): Boolean = {
      rows.exists(_.forall(_ == v)) ||
        cols.exists(_.forall(_ == v)) ||
        diagonals.exists(_.forall(_ == v))
    }
    if (wins(X)) Some(X)
    else if (wins(O)) Some(O)
    else if (board.contains(Empty)) None
    else Some(Empty)
  }

  def printBoard = {
    if (hasHumanPlayer) {
      rows.foreach { row =>
        printLine
        printRow(row)
      }
      printLine
    }
  }

  def printLine = println("-------")

  def printRow(row: Seq[Value]) = {
    row.foreach {
      case X => print("|X")
      case O => print("|O")
      case Empty => print("| ")
    }
    println("|")
  }

}
