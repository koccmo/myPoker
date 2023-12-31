package com.mypoker.domain

final case class OmahaHoldem(board: Board, hands: List[Hand])

object OmahaHoldem {

  val HandSize: Int = 4
}
