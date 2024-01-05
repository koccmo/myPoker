package com.mypoker.domain

sealed trait GameType

case object GameType {

  final case class TexasHoldem(board: Board, hands: List[Hand])

  case object TexasHoldem extends GameType {

    val HandSize: Int = 2
  }

  final case class FiveCardDraw(hands: List[Hand])

  case object FiveCardDraw extends GameType {

    val HandSize: Int = 5
  }

  final case class OmahaHoldem(board: Board, hands: List[Hand])

  object OmahaHoldem {

    val HandSize: Int = 4
  }
}