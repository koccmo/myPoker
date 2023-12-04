package com.mypoker.gametypes

import com.mypoker.domain.{Board, Hand}
import com.mypoker.validation.{ValidationError, Validator}
import com.mypoker.{Combination, Parse}

final case class TexasHoldem(board: Board, hands: List[Hand])

object TexasHoldem {

  private val parse: Parse = Parse()
  private val validate: Validate = Validate()

  implicit val handOrdering: Ordering[Hand] =
    Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

  def getAnswer(board: String, hands: List[String]): String = {
    val handsOrError: Either[ValidationError, List[Hand]] =
      validate.texasHoldem(board, hands)
        .map {
          case TexasHoldem(board, hands) =>
            hands
              .map(x => x.copy(strength = Some(Combination.getStrength(board.cards ++ x.cards))))
              .sorted
        }

    handsOrError.map(hands => parse(hands))
//      .fold(error => error.description, value => value)
    match {
      case Left(validationError) => validationError.description
      case Right(value)          => value
    }
  }
}
