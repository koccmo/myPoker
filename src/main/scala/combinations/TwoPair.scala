package combinations
import cards.Rank.{Ace, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}
import cards.{Card, CardRankValue, Rank}

case object TwoPair extends Combination {

  val firstPairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 1000, List(Three, Three) -> 2000,
    List(Four, Four) -> 3000, List(Five, Five) -> 4000, List(Six, Six) -> 5000, List(Seven, Seven) -> 6000,
    List(Eight, Eight) -> 7000, List(Nine, Nine) -> 8000, List(Ten, Ten) -> 9000, List(Jack, Jack) -> 10000,
    List(Queen, Queen) -> 11000, List(King, King) -> 12000, List(Ace, Ace) -> 13000)

  val secondPairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 50, List(Three, Three) -> 100, List(Four, Four) -> 150,
    List(Five, Five) -> 200, List(Six, Six) -> 250, List(Seven, Seven) -> 300, List(Eight, Eight) -> 350,
    List(Nine, Nine) -> 400, List(Ten, Ten) -> 450, List(Jack, Jack) -> 500, List(Queen, Queen) -> 550,
    List(King, King) -> 600, List(Ace, Ace) -> 650)

  override def startCombValue: Int = 2000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 2)
      .values
      .toList
      .length >= 2
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val allCardsRanks: List[Rank] = listOfCards
      .map(_.rank)
      .sortBy(CardRankValue.cardRankValue)

    val listOfPairRank: List[List[Rank]] = listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 2)
      .values
      .toList
      .map(_.map(_.rank))
      .sortBy(firstPairValue)

    if(listOfPairRank.length == 3) {
      val listStrongestTwoPairRank: List[List[Rank]] = listOfPairRank.drop(1)
      val topRankOfRest: List[Rank] = allCardsRanks.diff(listStrongestTwoPairRank.flatten).reverse.take(1)

      startCombValue +
        secondPairValue(listStrongestTwoPairRank.head) +
        firstPairValue(listStrongestTwoPairRank.last) +
        CardRankValue.getValueOfCards(topRankOfRest)
    } else {
      val topRankOfRest: List[Rank] = allCardsRanks.diff(listOfPairRank.flatten).reverse.take(1)

      startCombValue +
         secondPairValue(listOfPairRank.head) +
         firstPairValue(listOfPairRank.last) +
         CardRankValue.getValueOfCards(topRankOfRest)
    }
  }
}
