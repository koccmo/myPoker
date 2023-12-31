package com.mypoker.services

import com.mypoker.domain.{FiveCardDraw, Hand, OmahaHoldem, TexasHoldem}
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
          case "omaha-holdem" :: board :: hands => omahaHoldemResult(board, hands)
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

      private def omahaHoldemResult(board: String, hands: List[String]): String = {
        val result =
          validate
            .omahaHoldem(board, hands)
            .map {
              case OmahaHoldem(board, hands) =>
                hands.map { hand =>
                  val combinedBoard        = board.cards.combinations(3).toList
                  val combinedHand         = hand.cards.combinations(2).toList
                  val combinations        = combinedBoard.flatMap(boardCombination =>
                    combinedHand.map(handCombination => boardCombination ++ handCombination)
                  )
                  val topValueCombinations = combinations.map(combination => calculateStrength(combination)).max

                  hand.copy(strength = Some(topValueCombinations))
                }.sorted
            }

        result match {
          case Left(error)  => error.description
          case Right(value) => parse(value)
        }
      }
    }
}
