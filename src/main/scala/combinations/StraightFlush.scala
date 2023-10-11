package combinations
import cards.Rank._
import cards.{Card, CardRankValue, Rank, Suit}

case object StraightFlush extends Combination {

  val straight: Map[List[Rank], Int] = Map(List(Ace, Two, Three, Four, Five) -> 1,
    List(Two, Three, Four, Five, Six) -> 2, List(Three, Four, Five, Six, Seven) -> 3,
    List(Four, Five, Six, Seven, Eight) -> 4, List(Five, Six, Seven, Eight, Nine) -> 5,
    List(Six, Seven, Eight, Nine, Ten) -> 6, List(Seven, Eight, Nine, Ten, Jack) -> 7,
    List(Eight, Nine, Ten, Jack, Queen) -> 8, List(Nine, Ten, Jack, Queen, King) -> 9,
    List(Ten, Jack, Queen, King, Ace) -> 10)

  override def startCombValue: Int = 8000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    val listOfRankFiveOrMoreSimilarSuite: List[Rank] =
      listOfCards
        .groupBy(_.suit)
        .filter(_._2.length >= 5)
        .values
        .flatMap(_.map(_.rank))
        .toList

    straight
      .keys
      .toList
      .map(_.intersect(listOfRankFiveOrMoreSimilarSuite))
      .exists(_.length == 5)
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val listOfRankFiveOrMoreSimilarSuite: List[Rank] =
      listOfCards
        .groupBy(_.suit)
        .filter(_._2.length >= 5)
        .values
        .flatMap(_.map(_.rank))
        .toList

    val listOFStraightPossibleComb: List[List[Rank]] =
      straight
        .keys.map(_.intersect(listOfRankFiveOrMoreSimilarSuite))
        .filter(_.length == 5)
        .toList

    val topValueOfStraightFlush: Int =
      listOFStraightPossibleComb
        .map(x => straight(x))
        .max

    startCombValue + topValueOfStraightFlush
  }
}
