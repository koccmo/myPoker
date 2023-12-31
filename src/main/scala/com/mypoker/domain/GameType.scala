package com.mypoker.domain

sealed trait GameType {
  def hands: List[Hand]
}

final case class FiveCardDraw(hands: List[Hand]) extends GameType

object FiveCardDraw {

  val FiveCardDrawHandStringLength: Int = 10
}

final case class OmahaHoldem(board: Board, hands: List[Hand]) extends GameType

object OmahaHoldem {

  val OmahaHoldemHandStringLength: Int = 8
}

final case class TexasHoldem(board: Board, hands: List[Hand]) extends GameType

object TexasHoldem {

  val boardStringLength: Int           = 10
  val TexasHoldemHandStringLength: Int = 4
}
