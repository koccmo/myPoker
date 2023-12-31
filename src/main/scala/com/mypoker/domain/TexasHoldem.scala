package com.mypoker.domain

final case class TexasHoldem(board: Board, hands: List[Hand])

object TexasHoldem {

  val HandSize: Int = 2
}

