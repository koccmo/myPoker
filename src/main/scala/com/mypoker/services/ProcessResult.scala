package com.mypoker.services

import com.mypoker.domain.{FiveCardDraw, Hand, TexasHoldem}
import com.mypoker.services.validation.{Validate, ValidationError}

trait ProcessResult {
  def apply(line: String): String
}

object ProcessResult {

  def apply(
    validate: Validate,
    calculateStrength: CalculateStrength,
    parse: Parse
  ): ProcessResult =
    new ProcessResult {

      def apply(line: String): String =
        line.split("\\s+").toList match {
          case "texas-holdem" :: board :: hands => texasResult(board, hands)
          case "omaha-holdem" :: board :: hands => "The solution doesn't support Omaha Hold'em"
          case "five-card-draw" :: hands        => fiveCardDrawResult(hands)
          case x :: _                           => "Unrecognized game type"
          case _                                => "Invalid input"
        }

      implicit val handOrdering: Ordering[Hand] =
        Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

      private def texasResult(board: String, hands: List[String]): String = {
        val result: Either[ValidationError, List[Hand]] =
          validate
            .texasHoldem(board, hands)
            .map {
              case TexasHoldem(board, hands) =>
                hands
                  .map(hand => hand.copy(strength = Some(calculateStrength(board.cards ++ hand.cards))))
                  .sorted
            }

        result match {
          case Left(error)  => error.description
          case Right(value) => parse(value)
        }
      }

      private def fiveCardDrawResult(hands: List[String]): String = {
        val result =
          validate
            .fiveCardDraw(hands)
            .map {
              case FiveCardDraw(hands) =>
                hands.map { hand => hand.copy(strength = Some(calculateStrength(hand.cards))) }.sorted
            }

        result match {
          case Left(error)  => error.description
          case Right(value) => parse(value)
        }
      }
    }
}
