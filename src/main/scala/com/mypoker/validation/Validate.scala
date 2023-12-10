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

      private def validateBoard(board: String): Either[ValidationError, Board] = {
        validateBoardSize(board).flatMap { cardList =>
          val result =
            cardList
              .map(validateCard)
              .foldLeft((Option.empty[ValidationError], List.empty[Card])) {
                case ((exception, cards), value) =>
                  value.fold(exception => (Some(exception), cards), card => (exception, cards :+ card))
              }

          val (exceptionOpt, cards) = result

          exceptionOpt match {
            case Some(value) => Left(value)
            case None => Right(Board(cards))
          }
        }
      }

      private def validateBoardSize(str: String): Either[ValidationError, List[String]] =
        if (str.length == 10) Right(str.grouped(2).toList)
        else Left(WrongBoardStringLength(str.length))

      private def validateHands(hands: List[String]): Either[ValidationError, List[Hand]] = {
        validateSize(hands).flatMap { handsCard =>
          val result = handsCard.map(validateCard).foldLeft((Option.empty[ValidationError], List.empty[Card])) {
            case ((error, cards), value) =>
              value.fold(exception => (Some(exception), cards), card => (error, cards :+ card))
          }

          val (exception, cards) = result

          exception match {
            case Some(value) => Left(value)
            case None => Right(cards.grouped(2).map(x => Hand(x)).toList)
          }
        }
      }

      private def validateSize(hands: List[String]): Either[ValidationError, List[String]] =
        if (hands.forall(_.length == 4)) Right(hands.flatMap(_.grouped(2)))
        else Left(WrongHandStringLength)

      private def validateCard(str: String): Either[ValidationError, Card] =
        str.split("").toList match {
          case r :: s :: Nil =>
            for {
              rank <- Rank.fromString(r)
              suit <- Suit.fromString(s)
            } yield Card(rank, suit)
          case _ => Left(WrongCardString)
        }
    }
}
