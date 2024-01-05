package com.mypoker.services.validation

sealed trait ValidationError {

  def description: String
}

object ValidationError {

  final case class IncorrectRank(s: String) extends ValidationError {
    override def description: String = s"Not correct Rank - ($s)"
  }

  final case class IncorrectSuit(s: String) extends ValidationError {
    override def description: String = s"Not correct Suit - ($s)"
  }

  final case object WrongCardString extends ValidationError {
    override def description: String = s"Wrong card string"
  }

  final case class WrongCardAmount(amount: Int) extends ValidationError {
    override def description: String = s"Error: wrong cards amount - $amount"
  }
}
