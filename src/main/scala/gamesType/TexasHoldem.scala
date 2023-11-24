package gamesType


import cards.{Board, Card, Hand}
import combinations.Combination
import validation.Validator

import scala.annotation.tailrec

object TexasHoldem {


  def createListOfTupleHandListOfCards(
    board: Board,
    hands: List[Hand],
    handsString: List[String]
  ): List[(String, List[Card])] = {

    def getListTuple(
      tableCards: List[Card],
      listOfHandCards: List[Hand],
      listHandCardString: List[String]
    ): List[(String, List[Card])] = {
      @tailrec
      def helper(
        tableCards: List[Card],
        listOfHandCards: List[Hand],
        listHandCardString: List[String],
        acc: List[(String, List[Card])]
      ): List[(String, List[Card])] = {

        val newAcc: List[(String, List[Card])] = listOfHandCards match {
          case head :: _ => acc :+ (listHandCardString.head -> (tableCards ::: head.cards))
          case _         => acc
        }

        listOfHandCards match {
          case _ :: tail => helper(tableCards, tail, listHandCardString.tail, newAcc)
          case _         => newAcc
        }
      }

      helper(tableCards, listOfHandCards, listHandCardString, List.empty)
    }

    getListTuple(board.cards, hands, handsString)
  }


  def createAnswerFromTuple(tuples: List[(String, Int)]): String = {
    val sortedTuples = tuples.sorted.sortBy(_._2)

    def createAnswer(sortedTuples: List[(String, Int)]): String = {
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

      helper(sortedTuples, "", 0)
    }

    createAnswer(sortedTuples)
  }

  // TODO: such nested construction is difficult to read, I would use for-comprehension here
  def getAnswer(board: Board, hands: List[Hand], listHand: List[String]): String = {
    createAnswerFromTuple(
      createListOfTupleHandListOfCards(board, hands, listHand)
        .map { case (string, cards) => (string, Combination.getCardValue(cards)) })
  }


}
