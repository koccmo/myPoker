package com.mypoker.services.validation

import com.mypoker.domain.GameType.{FiveCardDraw, OmahaHoldem, TexasHoldem}
import com.mypoker.domain._
import com.mypoker.services.validation.ValidationError.WrongCardString

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
          hands <- validate(hands)(validateHand(_, TexasHoldem.HandSize))
        } yield TexasHoldem(board, hands)

      def fiveCardDraw(hands: List[String]): Either[ValidationError, FiveCardDraw] =
        validate(hands)(validateHand(_, FiveCardDraw.HandSize)).map(hands => FiveCardDraw(hands))

      def omahaHoldem(board: String, hands: List[String]): Either[ValidationError, OmahaHoldem] =
        for {
          board <- validateBoard(board)
          hands <- validate(hands)(validateHand(_, OmahaHoldem.HandSize))
        } yield OmahaHoldem(board, hands)

      private def validateBoard(input: String): Either[ValidationError, Board] =
        for {
          cards <- validate(input.grouped(Card.StringLength).toList)(validateCard)
          _     <- validateCardsAmount(cards, Board.Size)
        } yield Board(cards)

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

      private def validateCardsAmount(input: List[Card], expectedAmount: Int): Either[ValidationError, Unit] =
        if (input.length == expectedAmount) Right()
        else Left(ValidationError.WrongCardAmount(input.length))

      private def validateHand(input: String, cardsAmount: Int): Either[ValidationError, Hand] =
        for {
          cards <- validate(input.grouped(Card.StringLength).toList)(validateCard)
          _     <- validateCardsAmount(cards, cardsAmount)
        } yield Hand(cards)
    }
}
