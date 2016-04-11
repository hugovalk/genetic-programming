package com.devdiscoveries.examples

import scala.io.StdIn
import scala.util.Random

/**
  * Example game 'Three in a row". This game needs an integer
  * input (between 1 - 9) and is therefore suitable as a genetic
  * programming challenge.
  */
object ThreeInARow extends App {

  val winningPlayer = new Game(new ConsolePlayer(X), new ConsolePlayer(O)).play
  println(s"The winner is: $winningPlayer")

}

sealed trait Value
case object X extends Value
case object O extends Value
case object Empty extends Value

trait Player {
  def readMove: Int
  def color: Value
  def isHuman: Boolean
}
case class ConsolePlayer(color: Value) extends Player {
  override val isHuman = true
  override def readMove = {
    println(s"Player $color, enter your move (1 - 9):")
    StdIn.readInt()
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
      playMove(currentPlayer)
      winner match {
        case Some(v) =>
          printBoard
          v
        case None =>
          if (currentPlayer == playerOne)
            currentPlayer = playerTwo
          else
            currentPlayer = playerOne
          loop
      }
    }
    loop
  }

  def playMove(player: Player) = {
    val move = player.readMove
    board = board.take(move - 1) ++ (player.color :: board.drop(move - 1).tail)
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
        printLine()
        printRow(row)
      }
      printLine()
    }
  }

  def printLine() = println("-------")

  def printRow(row: Seq[Value]) = {
    row.foreach{
      case X => print("|X")
      case O => print("|O")
      case Empty => print("| ")
    }
    println("|")
  }

}
