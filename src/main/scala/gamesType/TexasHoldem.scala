package gamesType


import cards.{Board, Hand}
import combinations.Combination
import validation.Validator

import scala.annotation.tailrec

object TexasHoldem {

  def createTuples(board: Board, hands: List[Hand]): List[(Hand, Int)] = {
    @tailrec
    def helper(board: Board, hands: List[Hand], acc: List[(Hand, Int)]): List[(Hand, Int)] = {
      val newAcc: List[(Hand, Int)] = hands match {
        case head :: _ => acc :+ (head, Combination.getCardValue(board.cards ++ head.cards))
        case Nil => acc
      }

      hands match {
        case _ :: tail => helper(board, tail, newAcc)
        case _ => newAcc
      }
    }

    helper(board, hands, List.empty)
  }

  def createAnswer(tuples: List[(Hand, Int)]): String = {
    val sortedTuples = tuples
      .map{ case (hand, value) => (hand.toString, value) }
      .sorted
      .sortBy { case (_, int) => int }

    def transform(tuples: List[(String, Int)]): String = {
      @tailrec
      def helper(
        list: List[(String, Int)],
        accAnswer: String,
        accValue: Int
      ): String =
        list match {
          case (comb, value) :: tail if accValue == 0     => helper(tail, accAnswer + s"$comb", value)
          case (comb, value) :: tail if accValue == value => helper(tail, accAnswer + s"=$comb", value)
          case (comb, value) :: tail if accValue != value => helper(tail, accAnswer + s" $comb", value)
          case _                                          => accAnswer
        }

      helper(tuples, "", 0)
    }

    transform(sortedTuples)
  }

  def getAnswer(board: String, hands: List[String]): String = {
    val result = for {
      board <- Validator.validateBoard(board)
      hands <- Validator.validateHands(hands)
    } yield createAnswer(createTuples(board, hands))

    result match {
      case Left(validationError) => validationError.description
      case Right(value) => value
    }
  }


}
