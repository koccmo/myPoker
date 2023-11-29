package com.mypoker.gametypes

import com.mypoker.validation.Validator
import com.mypoker.{Combination, Parser}

object TexasHoldem {

  def getAnswer(board: String, hands: List[String]): String = {
    val result = for {
      board <- Validator.validateBoard(board)
      hands <- Validator.validateHands(hands.sorted)
    } yield {
      val newHands = hands.map(x => x.copy(strength = Some(Combination.getCardValue(board.cards ++ x.cards))))

      val sortedHands = newHands.sortBy(x => x.strength.getOrElse(0))

      Parser.parse(sortedHands)
    }

    result match {
      case Left(validationError) => validationError.description
      case Right(value) => value
    }
  }

}
