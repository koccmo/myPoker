package combinations

import cards.{Card, CardRankValue, Rank}

// TODO: shouldn't it extend Combination? I would try to reimplement it that way as HighCard would be Combination
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
