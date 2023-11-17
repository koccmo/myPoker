package cards

final case class Hands (cards: List[List[Card]]) {
  def getCards: List[List[Card]] = cards
}
