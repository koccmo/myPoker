package com.mypoker.validation

import com.mypoker.domain.{Board, Card, Hand, Rank, Suit}
import ValidationError.{WrongBoardStringLength, WrongCardString, WrongHandStringLength}
import com.mypoker.gametypes.TexasHoldem

object Validator {

  def validateTexasHoldem(board: String, hands: List[String]): Either[ValidationError, TexasHoldem] =
    for {
      board <- Validator.validateBoard(board)
      hands <- Validator.validateHands(hands)
    } yield TexasHoldem(board, hands)

  def validateCard(str: String): Either[ValidationError, Card] =
    str.split("").toList match {
      case r :: s :: Nil =>
        for {
          rank <- Rank.fromString(r)
          suit <- Suit.fromString(s)
        } yield Card(rank, suit)
      case _ => Left(WrongCardString)
    }

  def validateBoard(board: String): Either[ValidationError, Board] = {
    def validateBoardSize(str: String): Either[ValidationError, List[String]] =
      if (str.length == 10) Right(str.grouped(2).toList)
      else Left(WrongBoardStringLength(str.length))

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

  def validateHands(hands: List[String]): Either[ValidationError, List[Hand]] = {
    def validateSize(hands: List[String]): Either[ValidationError, List[String]] =
      if (hands.forall(_.length == 4)) Right(hands.flatMap(_.grouped(2)))
      else Left(WrongHandStringLength)

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
}
