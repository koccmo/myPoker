package com.mypoker.validation

import com.mypoker.domain._
import com.mypoker.gametypes.TexasHoldem
import com.mypoker.validation.ValidationError.{WrongBoardStringLength, WrongCardString, WrongHandStringLength}

trait Validate {
  def texasHoldem(board: String, hands: List[String]): Either[ValidationError, TexasHoldem]
}

object Validate {
  def apply(): Validate =
    new Validate {

      def texasHoldem(board: String, hands: List[String]): Either[ValidationError, TexasHoldem] =
        for {
          board <- validateBoard(board)
          hands <- validateHands(hands)
        } yield TexasHoldem(board, hands)

      private def validateBoard(board: String): Either[ValidationError, Board] =
        validateBoardSize(board).flatMap { cardList =>
          val result =
            cardList
              .map(validateCard)
              .foldLeft((Option.empty[ValidationError], List.empty[Card])) {
                case ((validationError, cards), value) =>
                  value.fold(
                    validationError => (Some(validationError), cards),
                    card => (validationError, cards :+ card)
                  )
              }

          val (validationError, cards) = result

          validationError match {
            case Some(value) => Left(value)
            case None        => Right(Board(cards))
          }
        }

      private def validateBoardSize(str: String): Either[ValidationError, List[String]] =
        if (str.length == 10) Right(str.grouped(2).toList)
        else Left(WrongBoardStringLength(str.length))

      private def validateHands(hands: List[String]): Either[ValidationError, List[Hand]] = {
        val validatedHands = hands.map(validateHand)
        val result = validatedHands.foldLeft((Option.empty[ValidationError], List.empty[Hand])) {
          case ((validationError, hands), value) =>
            value.fold(
              validationError => (Some(validationError), hands),
              card => (validationError, hands :+ card)
            )
        }

        result match {
          case (Some(value), _) => Left(value)
          case (None, hands)    => Right(hands)
        }
      }

      private def validateHand(hand: String): Either[ValidationError, Hand] = {
        val cards = for {
          validated <- validateHandSize(hand)
          grouped    = validated.grouped(2).toList
        } yield grouped.map(validateCard)

        cards.flatMap { handCards =>
          val result = handCards.foldLeft((Option.empty[ValidationError], List.empty[Card])) {
            case ((validationError, cards), value) =>
              value.fold(validationError => (Some(validationError), cards), card => (validationError, cards :+ card))
          }

          val (validationError, cards) = result

          validationError match {
            case Some(error) => Left(error)
            case None        => Right(Hand(cards))
          }
        }
      }

      private def validateHandSize(hand: String): Either[ValidationError, String] =
        if (hand.length == 4) Right(hand)
        else Left(WrongHandStringLength)

      private def validateCard(str: String): Either[ValidationError, Card] =
        str.split("").toList match {
          case r :: s :: Nil =>
            for {
              rank <- Rank.fromString(r)
              suit <- Suit.fromString(s)
            } yield Card(rank, suit)
          case _             => Left(WrongCardString)
        }
    }
}
