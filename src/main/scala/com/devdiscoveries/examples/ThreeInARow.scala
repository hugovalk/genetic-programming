package com.devdiscoveries.examples

import com.devdiscoveries.genprog.{Operation, Param, Node}

import scala.io.StdIn
import scala.util.Random

/**
  * Example game 'Three in a row". This game needs an integer
  * input (between 1 - 9) and is therefore suitable as a genetic
  * programming challenge.
  */
object ThreeInARow extends App {
//  val winningPlayer = new Game(new ConsolePlayer(X), new ConsolePlayer(O)).play
  import Operation._
  val tree =
    Node.generateRandomTree[Int](params = List(Param("0"),
    Param("1"), Param("2"), Param("3"), Param("4"),
    Param("5"), Param("6"), Param("7"), Param("8")),
    operations = List(add[Int], subtract[Int], multiply[Int], greaterThan[Int]),
    valueGenerator = () => Random.nextInt)
  val winningPlayer = new Game(new ConsolePlayer(X), new GPPlayer(O,tree)).play
  println(s"The winner is: $winningPlayer")
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
    val params = board.zipWithIndex.map{ e => e._1 match {
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
    List(board(2), board(4), board(6))
  )

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
