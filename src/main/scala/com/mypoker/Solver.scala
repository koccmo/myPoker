package com.mypoker

import com.mypoker.domain.{Hand, TexasHoldem}
import com.mypoker.validation.{Validate, ValidationError}

object Solver {

  final val parse: Parse = Parse()
  final val validate: Validate = Validate()
  final val getStrength: CalculateCombinationStrength = CalculateCombinationStrength()

  implicit val handOrdering: Ordering[Hand] =
    Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

  def process(line: String): String = {

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => texasResult(board, hands)
      case "omaha-holdem" :: board :: hands => "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => "The solution doesn't support Five Card Draw"
      case x :: _ => "Unrecognized game type"
      case _ => "Invalid input"
    }
  }

  private def texasResult(board: String, hands: List[String]): String = {
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
      case Left(error) => error.description
      case Right(value) => parse(value)
    }
  }
}
