package com.mypoker.validation

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

  final case class WrongBoardStringLength(length: Int) extends ValidationError {
    override def description: String = s"Error: wrong board string length - ($length)"
  }

  final case object WrongHandStringLength extends ValidationError {
    override def description: String = "Error: wrong hand string length"
  }

  final case object WrongCardString extends ValidationError {
    override def description: String = s"Wrong card string"
  }
}
