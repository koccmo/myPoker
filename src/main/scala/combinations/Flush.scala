package combinations
import cards.{Card, CardRankValue, Rank}

case object Flush extends Combination {
  override def startCombValue: Int = 5000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    listOfCards
      .groupBy(_.suit)
      .exists(_._2.length >= 5)
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val topFiveRankCardSimilarSuits: List[Rank] = listOfCards
      .groupBy(_.suit)
      .filter(_._2.length >= 5)
      .flatMap(_._2.map(_.rank))
      .toList
      .sortBy(CardRankValue.cardRankValue)
      .reverse
      .take(5)

    startCombValue + CardRankValue.getValueOfCards(topFiveRankCardSimilarSuits)
  }
}
