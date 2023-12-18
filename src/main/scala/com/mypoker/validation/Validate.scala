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

      private def validateBoard(input: String): Either[ValidationError, Board] =
        for {
          boardString    <- validateBoardSize(input)
          cards           = boardString.grouped(2).toList
          validatedCards <- validate(cards)(validateCard)
        } yield Board(validatedCards)

      private def validateBoardSize(str: String): Either[ValidationError, String] =
        if (str.length == 10) Right(str)
        else Left(WrongBoardStringLength(str.length))

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

      private def validateHands(input: List[String]): Either[ValidationError, List[Hand]] =
        validate(input)(validateHand)

      private def validateHand(input: String): Either[ValidationError, Hand] =
        for {
          handString     <- validateHandSize(input)
          cards           = handString.grouped(2).toList
          validatedCards <- validate(cards)(validateCard)
        } yield Hand(validatedCards)

      private def validateHandSize(str: String): Either[ValidationError, String] =
        if (str.length == 4) Right(str)
        else Left(WrongHandStringLength)

    }
}
