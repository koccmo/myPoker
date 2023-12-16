package com.mypoker

import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._
import com.mypoker.validation.ValidationError

object Myyyy extends App {

  val ok = List("4s8d", "Qh9s")
  println(ok.flatMap(_.grouped(2).toList))

  println("a2k3".grouped(2).toList)

  def validate[T](
    items: List[String]
  )(
    function: String => Either[ValidationError, T]
  ): Either[ValidationError, List[T]] = {
    val result = items.map(function).foldLeft((Option.empty[ValidationError], List.empty[T])) {
      case ((validationError, cards), value) =>
        value.fold(validationError => (Some(validationError), cards), card => (validationError, cards :+ card))
    }

    result match {
      case (Some(error), _) => Left(error)
      case (None, cards)    => Right(cards)
    }
  }
}
