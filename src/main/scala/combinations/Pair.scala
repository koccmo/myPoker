package combinations

import cards.Rank.{Ace, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}
import cards.{Card, CardRankValue, Rank}

case object Pair extends Combination {
  val pairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 10000, List(Three, Three) -> 20000, List(Four, Four) -> 30000,
    List(Five, Five) -> 40000, List(Six, Six) -> 50000, List(Seven, Seven) -> 60000, List(Eight, Eight) -> 70000,
    List(Nine, Nine) -> 80000, List(Ten, Ten) -> 90000, List(Jack, Jack) -> 100000, List(Queen, Queen) -> 110000,
    List(King, King) -> 120000, List(Ace, Ace) -> 130000)

  override val startCombValue: Int = 1000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 2)
      .toList
      .length == 1
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val allCardsRanks: List[Rank] = listOfCards
      .map(_.rank)
      .sortBy(CardRankValue.cardRankValue)


    val pair: List[Rank] = listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 2)
      .flatMap(_._2)
      .map(_.rank)
      .toList

    val allCardRanksWithoutPair: List[Rank] = allCardsRanks
      .diff(pair)
      .drop(2)
      .reverse

    startCombValue + pairValue(pair) + CardRankValue.getValueOfCards(allCardRanksWithoutPair)
  }
}
