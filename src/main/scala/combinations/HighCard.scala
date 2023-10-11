package combinations

import cards.{Card, CardRankValue, Rank}

case object HighCard {
  def getValueOfComb(listOfCards: List[Card]): Int = {
    val allCardRank: List[Rank] = listOfCards
      .map(x => x.rank)
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(5)

    CardRankValue.getValueOfCards(allCardRank)
  }
}
