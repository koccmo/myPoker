package com.mypoker.services.validation

import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._
import com.mypoker.domain._
import ValidationError.{WrongBoardStringLength, WrongCardString, WrongHandStringLength}

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
          hands <- validate(hands)(validateHand(_, 4))
        } yield TexasHoldem(board, hands)

      def fiveCardDraw(hands: List[String]): Either[ValidationError, FiveCardDraw] =
        for {
          hands <- validate(hands)(validateHand(_,10))
        } yield FiveCardDraw(hands)

     def omahaHoldem(board: String, hands: List[String]): Either[ValidationError, OmahaHoldem] =
       for {
         board <- validateBoard(board)
         hands <- validate(hands)(validateHand(_, 8))
       } yield OmahaHoldem(board, hands)

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
        RanksMap.get(s) match {
          case Some(value) => Right(value)
          case None        => Left(ValidationError.IncorrectRank(s))
        }

      private def validateSuit(s: String): Either[ValidationError, Suit] =
        SuitsMap.get(s) match {
          case Some(value) => Right(value)
          case None        => Left(ValidationError.IncorrectSuit(s))
        }

      private def validateHand(input: String, expectedLength: Int): Either[ValidationError, Hand] =
        for {
          handString     <- validateHandSize(input, expectedLength)
          cards           = handString.grouped(2).toList
          validatedCards <- validate(cards)(validateCard)
        } yield Hand(validatedCards)

      private def validateHandSize(str: String, expectedLength: Int): Either[ValidationError, String] =
        if (str.length == expectedLength) Right(str)
        else Left(WrongHandStringLength)
    }
}
