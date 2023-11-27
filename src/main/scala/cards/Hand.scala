package cards

final case class Hand(cards: List[Card]) {
  override def toString: String = cards.map(_.toString).mkString
}



