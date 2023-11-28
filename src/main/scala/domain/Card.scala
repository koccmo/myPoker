package domain

final case class Card(rank: Rank, suit: Suit) {
  override def toString: String = {
    rank.toString + suit.toString
  }
}
