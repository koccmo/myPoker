package com.mypoker.services

import com.mypoker.domain.{FiveCardDraw, GameType, Hand, OmahaHoldem, TexasHoldem}
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
          case "texas-holdem" :: board :: hands => process(board, hands)(validate.texasHoldem)
          case "omaha-holdem" :: board :: hands => process(board, hands)(validate.omahaHoldem)
          case "five-card-draw" :: hands        => process("", hands)((_, hands) => validate.fiveCardDraw(hands))
          case gameType :: _                    => s"Unrecognized game type $gameType"
          case _                                => "Invalid input"
        }

      implicit val handOrdering: Ordering[Hand] =
        Ordering.by[Hand, Int](_.strength.getOrElse(0)) orElse Ordering.by[Hand, String](_.toString)

      private def process(
        board: String,
        hands: List[String]
      )(
        validateGameType: (String, List[String]) => Either[ValidationError, GameType]
      ): String = {
        val result: Either[ValidationError, List[Hand]] =
          validateGameType(board, hands)
            .map { gameType =>
              gameType.hands
                .map(hand => hand.copy(strength = Some(getHandStrength(gameType, hand))))
                .sorted
            }

        result match {
          case Left(error) => error.description
          case Right(value) => parse(value)
        }
      }

      private def getHandStrength(gameType: GameType, hand: Hand): Int =
        gameType match {
          case _: FiveCardDraw       => calculateStrength(hand.cards)
          case TexasHoldem(board, _) => calculateStrength(board.cards ++ hand.cards)
          case OmahaHoldem(board, _) =>
            val handStrengths =
              for {
                boardCardCombination <- board.cards.combinations(3)
                handCardCombination  <- hand.cards.combinations(2)
              } yield calculateStrength(boardCardCombination ++ handCardCombination)

            handStrengths.max
        }
    }
}
