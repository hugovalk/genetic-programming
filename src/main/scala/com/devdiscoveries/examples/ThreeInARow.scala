package com.devdiscoveries.examples

import scala.io.StdIn
import scala.util.Random

/**
  * Created by hv01016 on 11-4-2016.
  */
object ThreeInARow extends App {

  sealed trait Value
  case object X extends Value
  case object O extends Value
  case object Empty extends Value

  var board: List[Value] = (1 to 9).toList.map(_ => Empty)

  printBoard
  val winningPlayer = play
  println(s"The winner is: $winningPlayer")

  def play: Value = {
    var currentPlayer = if (Random.nextFloat() < 0.5) X else O
    def loop: Value = {
      playConsoleMove(currentPlayer)
      winner match {
        case Some(v) => v
        case None =>
          currentPlayer match {
            case X => currentPlayer = O
            case O => currentPlayer = X
            case _ => throw new IllegalStateException("There must be a current player")
          }
          loop
      }
    }
    loop
  }

  def playConsoleMove(player: Value) = {
    println(s"Player $player, enter your move (1 - 9):")
    val choice = StdIn.readInt()
    board = board.take(choice - 1) ++ (player :: board.drop(choice - 1).tail)
    printBoard
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
    rows.foreach { row =>
      printLine()
      printRow(row)
    }
    printLine()
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
