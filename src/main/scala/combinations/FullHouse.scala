package combinations
import cards.Rank._
import cards.{Card, Rank}

case object FullHouse extends Combination {

  val threeOfKind: Map[List[Rank], Int] = Map(List(Two, Two, Two) -> 100, List(Three, Three, Three) -> 200,
    List(Four, Four, Four) -> 300, List(Five, Five, Five) -> 400, List(Six, Six, Six) -> 500,
    List(Seven, Seven, Seven) -> 600, List(Eight, Eight, Eight) -> 700, List(Nine, Nine, Nine) -> 800,
    List(Ten, Ten, Ten) -> 900, List(Jack, Jack, Jack) -> 1000, List(Queen, Queen, Queen) -> 1100,
    List(King, King, King) -> 1200, List(Ace, Ace, Ace) -> 1300)

  val pair: Map[List[Rank], Int] = Map(List(Two, Two) -> 1, List(Three, Three) -> 2, List(Four, Four) -> 3,
    List(Five, Five) -> 4, List(Six, Six) -> 5, List(Seven, Seven) -> 6, List(Eight, Eight) -> 7,
    List(Nine, Nine) -> 8, List(Ten, Ten) -> 9, List(Jack, Jack) -> 10, List(Queen, Queen) -> 11, List(King, King) -> 12,
    List(Ace, Ace) -> 13)

  override def startCombValue: Int = 6000000

  override def checkComb(listOfCards: List[Card]): Boolean = {
    val mapRankListCardWithSimilarRank: Map[Rank, List[Card]] = listOfCards.groupBy(_.rank)
    val numberOfTreeOfKind: Int = mapRankListCardWithSimilarRank.count(_._2.length == 3)
    val numberOfPair: Int = mapRankListCardWithSimilarRank.count(_._2.length == 2)

    numberOfTreeOfKind == 2 || (numberOfTreeOfKind == 1 && numberOfPair >= 1)
  }

  override def getValueOfComb(listOfCards: List[Card]): Int = {
    val listOfThreeSimilarRanks: List[List[Rank]] = listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 3)
      .values
      .toList
      .map(_.map(_.rank))

    if (listOfThreeSimilarRanks.length == 1) {
      val twoSimilarRanksTopValue: List[List[Rank]] = listOfCards
        .groupBy(_.rank)
        .filter(_._2.length == 2)
        .values
        .toList
        .map(_.map(_.rank))
        .sortBy(pair)
        .reverse
        .take(1)

      startCombValue + threeOfKind(listOfThreeSimilarRanks.head) + pair(twoSimilarRanksTopValue.head)
    } else {
      val topValueTheeSimilarRanks: List[Rank] = {
        listOfThreeSimilarRanks
          .maxBy(threeOfKind)
      }

      val pairRankList: List[Rank] = {
        listOfThreeSimilarRanks
          .minBy(threeOfKind)
          .take(2)
      }

      startCombValue + threeOfKind(topValueTheeSimilarRanks) + pair(pairRankList)
    }
  }
}
