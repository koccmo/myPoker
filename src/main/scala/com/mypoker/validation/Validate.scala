package com.mypoker.validation

import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._
import com.mypoker.domain._
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
          hands <- validate(hands)(validateHand)
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
        val result = items.map(function)
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
        s match {
          case "2" => Right(Two)
          case "3" => Right(Three)
          case "4" => Right(Four)
          case "5" => Right(Five)
          case "6" => Right(Six)
          case "7" => Right(Seven)
          case "8" => Right(Eight)
          case "9" => Right(Nine)
          case "T" => Right(Ten)
          case "J" => Right(Jack)
          case "Q" => Right(Queen)
          case "K" => Right(King)
          case "A" => Right(Ace)
          case _ => Left(ValidationError.IncorrectRank(s))
        }

      private def validateSuit(s: String): Either[ValidationError, Suit] = s match {
        case "h" => Right(Hearts)
        case "d" => Right(Diamonds)
        case "c" => Right(Clubs)
        case "s" => Right(Spades)
        case _ => Left(ValidationError.IncorrectSuit(s))
      }

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
