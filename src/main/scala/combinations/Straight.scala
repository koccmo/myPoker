package combinations
import cards.Rank._
import cards.{Card, CardRankValue, Rank}

case object Straight extends Combination {

  val straight: Map[List[Rank], Int] = Map(List(Ace, Two, Three, Four, Five) -> 1,
    List(Two, Three, Four, Five, Six) -> 2, List(Three, Four, Five, Six, Seven) -> 3,
    List(Four, Five, Six, Seven, Eight) -> 4, List(Five, Six, Seven, Eight, Nine) -> 5,
    List(Six, Seven, Eight, Nine, Ten) -> 6, List(Seven, Eight, Nine, Ten, Jack) -> 7,
    List(Eight, Nine, Ten, Jack, Queen) -> 8, List(Nine, Ten, Jack, Queen, King) -> 9,
    List(Ten, Jack, Queen, King, Ace) -> 10)

  override def startCombValue: Int = 4000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    val listOfAllStraightComb: List[List[Rank]] = straight.keySet.toList
    val cardRank: List[Rank] = listOfCards.map(_.rank).sortBy(CardRankValue.cardRankValue)

    listOfAllStraightComb.map(_.diff(cardRank)).exists(_.isEmpty)
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val listOfAllStraightComb: List[List[Rank]] = straight
      .keys
      .toList

    val cardRanks: List[Rank] = listOfCards.map(_.rank)

    val listFoundedComb: List[List[Rank]] = listOfAllStraightComb
      .map(_.intersect(cardRanks))
      .filter(_.length == 5)

    val valueOfTopFoundedComb: Int = listFoundedComb
      .map(x => straight(x))
      .max

    startCombValue + valueOfTopFoundedComb
  }
}
