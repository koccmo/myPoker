package com.mypoker.gametypes

import com.mypoker.domain.{Board, Hand}
import com.mypoker.validation.{Validate, ValidationError}
import com.mypoker.{Combination, Parse}

final case class TexasHoldem(board: Board, hands: List[Hand])

object TexasHoldem {

  final val parse: Parse       = Parse()
  final val validate: Validate = Validate()

  implicit val handOrdering: Ordering[Hand] =
    Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

  def getAnswer(board: String, hands: List[String]): String = {
    val ansOrError: Either[ValidationError, List[Hand]] =
      validate.validateTexasHoldem(board, hands)
        .map {
          case TexasHoldem(board, hands) =>
            hands
              .map(x => x.copy(strength = Some(Combination.getStrength(board.cards ++ x.cards))))
              .sorted
        }

    ansOrError match {
      case Left(error)  => error.description
      case Right(value) => parse(value)
    }
  }
}
