package com.mypoker.gametypes

import com.mypoker.domain.Hand
import com.mypoker.validation.Validator
import com.mypoker.{Combination, Parser}

object TexasHoldem {

  implicit val handOrdering: Ordering[Hand] =
    Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

  def getAnswer(board: String, hands: List[String]): String = {
    val result = for {
      board <- Validator.validateBoard(board)
      hands <- Validator.validateHands(hands)
    } yield {
      val newHands =
        hands
          .map(x => x.copy(strength = Some(Combination.getStrength(board.cards ++ x.cards))))
          .sorted

      Parser.parse(newHands)
    }

    result match {
      case Left(validationError) => validationError.description
      case Right(value) => value
    }
  }
}
