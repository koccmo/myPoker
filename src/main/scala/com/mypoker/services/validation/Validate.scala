package com.mypoker.services.validation

import com.mypoker.domain._
import com.mypoker.services.validation.ValidationError.{WrongCardString, WrongHandLength}

trait Validate {

  def texasHoldem(board: String, hands: List[String]): Either[ValidationError, TexasHoldem]
  def fiveCardDraw(hands: List[String]): Either[ValidationError, FiveCardDraw]
  def omahaHoldem(board: String, hands: List[String]): Either[ValidationError, OmahaHoldem]
}

object Validate {

  def apply(): Validate =
    new Validate {

      def texasHoldem(board: String, hands: List[String]): Either[ValidationError, TexasHoldem] =
        for {
          board <- validateBoard(board)
          hands <- validateHands(hands, TexasHoldem.HandSize)
        } yield TexasHoldem(board, hands)

      def fiveCardDraw(hands: List[String]): Either[ValidationError, FiveCardDraw] =
        for {
          hands <- validateHands(hands, FiveCardDraw.HandSize)
        } yield FiveCardDraw(hands)

      def omahaHoldem(board: String, hands: List[String]): Either[ValidationError, OmahaHoldem] =
        for {
          board <- validateBoard(board)
          hands <- validateHands(hands, OmahaHoldem.HandSize)
        } yield OmahaHoldem(board, hands)

      private def validateBoard(input: String): Either[ValidationError, Board] =
        for {
          cards          <- validateCards(input)
          validatedCards <- validateBoardSize(cards, Board.Size)
        } yield Board(validatedCards)

      private def validateCards(input: String): Either[ValidationError, List[Card]] = {
        validate(input.grouped(Card.StringLength).toList)(validateCard)
      }

      private def validateBoardSize(input: List[Card], expectedLength: Int): Either[ValidationError, List[Card]] =
        if (input.length == expectedLength) Right(input)
        else Left(ValidationError.WrongBoardLength(input.length))

      private def validate[T, A](
        items: List[A]
      )(
        function: A => Either[ValidationError, T]
      ): Either[ValidationError, List[T]] = {
        val result = items
          .map(function)
          .foldLeft((Option.empty[ValidationError], List.empty[T])) {
            case ((error, items), value) =>
              value.fold(error => (Some(error), items), item => (error, items :+ item))
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
              rank <- validateRank(r)
              suit <- validateSuit(s)
            } yield Card(rank, suit)
          case _             => Left(WrongCardString)
        }

      private def validateRank(s: String): Either[ValidationError, Rank] =
        Rank.ValuesMap.get(s).toRight(ValidationError.IncorrectRank(s))

      private def validateSuit(s: String): Either[ValidationError, Suit] =
        Suit.ValuesMap.get(s).toRight(ValidationError.IncorrectSuit(s))

      private def validateHands(input: List[String], handLength: Int): Either[ValidationError, List[Hand]] =
        for {
          cards <- validate(input)(validateCards)
          hands <- validate(cards)(validateHandSize(_, handLength))
        } yield hands

      private def validateHandSize(input: List[Card], expectedSize: Int): Either[ValidationError, Hand] =
        if (input.length == expectedSize) Right(Hand(input))
        else Left(WrongHandLength(input.length))
    }
}
