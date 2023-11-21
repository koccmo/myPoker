package cards

// TODO: This model is ugly. You can wrap cards of one hand like:
//       final case class Hand(cards: List[Card])
//       And in code when you have hands it will look like hands: List[Hand]

final case class Hand(cards: List[Card])



