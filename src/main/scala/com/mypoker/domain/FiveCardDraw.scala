package com.mypoker.domain

final case class FiveCardDraw(hands: List[Hand])

object FiveCardDraw {

  val HandSize: Int = 5
}
