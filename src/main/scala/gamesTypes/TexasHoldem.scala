package gamesTypes

import domain.{Board, Hand}
import validation.Validator

import scala.annotation.tailrec

object TexasHoldem {

  def createAnswer(board: Board, hands: List[Hand]): String = {
    val newHands = hands.map(x => x.setStrength(Combination.getCardValue(board.cards ++ x.cards)))
    val sortedHands = newHands.sortBy(x => x.strength.getOrElse(0))

    def createString(hands: List[Hand]): String = {
      @tailrec
      def helper(hands: List[Hand], accAns: String, accValue: Int): String = {
        hands match {
          case head :: tail
            if accValue == 0                          => helper(tail, accAns + s"${head.toString}", head.strength.getOrElse(0))
          case head :: tail
            if accValue == head.strength.getOrElse(0) => helper(tail, accAns + s"=${head.toString}", head.strength.getOrElse(0))
          case head :: tail
            if accValue != head.strength.getOrElse(0) => helper(tail, accAns + s" ${head.toString}", head.strength.getOrElse(0))
          case _                                      => accAns
        }
      }

      helper(hands, "", 0)
    }

    createString(sortedHands)
  }

  def getAnswer(board: String, hands: List[String]): String = {
    val result = for {
      board <- Validator.validateBoard(board)
      hands <- Validator.validateHands(hands.sorted)
    } yield createAnswer(board, hands)

    result match {
      case Left(validationError) => validationError.description
      case Right(value) => value
    }
  }


}
