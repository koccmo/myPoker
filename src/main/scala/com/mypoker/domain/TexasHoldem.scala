package com.mypoker.domain

final case class TexasHoldem(board: Board, hands: List[Hand])

object TexasHoldem {
  final val boardStringLength: Int = 10
  val TexasHoldemHandStringLength: Int = 4
}

