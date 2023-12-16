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
        for {
          validatedSize <- validateBoardSize(board)
          board         <- validate(validatedSize)(validateCard)
        } yield Board(board)

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

      private def validateHand(hand: String): Either[ValidationError, Hand] =
        for {
          validated <- validateHandSize(hand)
          grouped    = validated.grouped(2).toList
          cards     <- validate(grouped)(validateCard)
        } yield Hand(cards)

      private def validateHandSize(hand: String): Either[ValidationError, String] =
        if (hand.length == 4) Right(hand)
        else Left(WrongHandStringLength)

      private def validate[T](
        items: List[String]
      )(
        function: String => Either[ValidationError, T]
      ): Either[ValidationError, List[T]] = {
        val result = items.map(function).foldLeft((Option.empty[ValidationError], List.empty[T])) {
          case ((validationError, items), value) =>
            value.fold(validationError => (Some(validationError), items), item => (validationError, items :+ item))
        }

        result match {
          case (Some(error), _) => Left(error)
          case (None, items)    => Right(items)
        }
      }

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
