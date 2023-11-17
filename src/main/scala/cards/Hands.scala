package cards

// TODO: This model is ugly. You can wrap cards of one hand like:
//       final case class Hand(cards: List[Card])
//       And in code when you have hands it will look like hands: List[Hand]

final case class Hands (cards: List[List[Card]]) {
  // TODO: What the point to have such method? It's useless. Just use Hands.cards instead
  def getCards: List[List[Card]] = cards
}
