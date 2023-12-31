package com.mypoker.domain

final case class Board(cards: List[Card])

object Board {

  val Size: Int = 5
}
