package combinations
import cards.Rank._
import cards.{Card, CardRankValue, Rank}

case object ThreeOfKind extends Combination {

  val threeOfKind: Map[List[Rank], Int] = Map(List(Two, Two, Two) -> 1000, List(Three, Three, Three) -> 2000,
    List(Four, Four, Four) -> 3000, List(Five, Five, Five) -> 4000, List(Six, Six, Six) -> 5000,
    List(Seven, Seven, Seven) -> 6000, List(Eight, Eight, Eight) -> 7000, List(Nine, Nine, Nine) -> 8000,
    List(Ten, Ten, Ten) -> 9000, List(Jack, Jack, Jack) -> 10000, List(Queen, Queen, Queen) -> 11000,
    List(King, King, King) -> 12000, List(Ace, Ace, Ace) -> 13000)

  override def startCombValue: Int = 3000000

  override def checkComb(listOfCards: List[Card]): Boolean =
    listOfCards
    .groupBy(_.rank)
    .exists(_._2.length == 3)

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val cardRankList: List[Rank] = listOfCards
      .map(_.rank)
      .sortBy(CardRankValue.cardRankValue)

    val listOfTopThreeOfKindsRank: List[Rank] = listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 3)
      .values
      .map(_.map(_.rank))
      .toList
      .sortBy(threeOfKind)
      .reverse
      .take(1).flatten

    val twoTopRankOfRest: List[Rank] = cardRankList
      .diff(listOfTopThreeOfKindsRank)
      .reverse
      .take(2)

    startCombValue +
      threeOfKind(listOfTopThreeOfKindsRank) +
      CardRankValue.getValueOfCards(twoTopRankOfRest)
  }
}
