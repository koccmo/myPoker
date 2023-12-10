package com.mypoker.gametypes

import com.mypoker.domain.{Board, Hand}
import com.mypoker.validation.{Validate, ValidationError}
import com.mypoker.{CalculateCombinationStrength, Parse}

final case class TexasHoldem(board: Board, hands: List[Hand])

object TexasHoldem {

  final val parse: Parse       = Parse()
  final val validate: Validate = Validate()
  final val getStrength: CalculateCombinationStrength = CalculateCombinationStrength()

  implicit val handOrdering: Ordering[Hand] =
    Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

  def getAnswer(board: String, hands: List[String]): String = {
    val result: Either[ValidationError, List[Hand]] =
      validate
        .texasHoldem(board, hands)
        .map {
          case TexasHoldem(board, hands) =>
            hands
              .map(hand => hand.copy(strength = Some(getStrength(board.cards ++ hand.cards))))
              .sorted
        }

    result match {
      case Left(error)  => error.description
      case Right(value) => parse(value)
    }
  }
}
