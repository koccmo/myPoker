package com.mypoker.domain

final case class FiveCardDraw(hands: List[Hand])

object FiveCardDraw {

  val FiveCardDrawHandStringLength: Int = 10
}
