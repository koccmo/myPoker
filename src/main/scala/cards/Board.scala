package cards

final case class Board(cards: List[Card]) {
  // TODO: What the point to have such method?
  def getCards: List[Card] = cards
}
