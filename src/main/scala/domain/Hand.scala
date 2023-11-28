package domain

final case class Hand(cards: List[Card], strength: Option[Int] = None ) {
  override def toString: String = cards.map(_.toString).mkString
  def setStrength(newStrength: Int): Hand = copy(strength = Some(newStrength))
}



