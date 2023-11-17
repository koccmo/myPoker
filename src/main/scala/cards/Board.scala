package cards

final case class Board(cards: List[Card]) {
  def getCards: List[Card] = cards
}
