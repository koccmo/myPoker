package combinations
import cards.Rank._
import cards.{Card, CardRankValue, Rank}

case object FourOfKind extends Combination {

  val fourOfAKind: Map[List[Rank], Int] = Map(List(Two, Two, Two, Two) -> 100, List(Three, Three, Three, Three) -> 200,
    List(Four, Four, Four, Four) -> 300, List(Five, Five, Five, Five) -> 400, List(Six, Six, Six, Six) -> 500,
    List(Seven, Seven, Seven, Seven) -> 600, List(Eight, Eight, Eight, Eight) -> 700, List(Nine, Nine, Nine, Nine) -> 800,
    List(Ten, Ten, Ten, Ten) -> 900, List(Jack, Jack, Jack, Jack) -> 1000, List(Queen, Queen, Queen, Queen) -> 1100,
    List(King, King, King, King) -> 1200, List(Ace, Ace, Ace, Ace) -> 1300)

  override def startCombValue: Int = 7000000

  override def checkComb(listOfCards: List[Card]): Boolean =
    listOfCards
      .groupBy(_.rank)
      .exists(_._2.length == 4)


  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val rankListFourOfKind: List[Rank] =
      listOfCards
        .groupBy(_.rank)
        .filter(_._2.length == 4)
        .values
        .toList
        .flatMap(_.map(_.rank))

    val restRankOfCards: List[Rank] =
      listOfCards
        .map(_.rank)
        .diff(rankListFourOfKind)
        .sortBy(CardRankValue.cardRankValue)
        .reverse
        .take(1)

    startCombValue + fourOfAKind(rankListFourOfKind) + CardRankValue.getValueOfCards(restRankOfCards)
  }
}


