package com.mypoker.domain

final case class Hand(cards: List[Card], strength: Option[Int] = None ) {
  override def toString: String = cards.map(_.toString).mkString
}



