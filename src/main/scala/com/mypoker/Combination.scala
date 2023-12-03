package com.mypoker

import com.mypoker.domain.Rank._
import com.mypoker.domain.{Card, Rank}

sealed trait Combination {

  def getCombStrength(listOfCards: List[Card]): Int

  def checkComb(listOfCards: List[Card]): Boolean

}

object Combination {

  val listOfCombination: List[Combination] = List(
    StraightFlush,
    FourOfKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfKind,
    TwoPair,
    Pair,
    HighCard
  )

  def getStrength(cardList: List[Card]): Int = {
    listOfCombination
      .collectFirst {
        case comb if comb.checkComb(cardList) => comb.getCombStrength(cardList)
      }
      .getOrElse(HighCard.getCombStrength(cardList))
  }

  case object HighCard extends Combination {
    override def getCombStrength(listOfCards: List[Card]): Int = {
      val allCardRank: List[Rank] = listOfCards
        .map(x => x.rank)
        .sortBy(CardValue.cardRankValue)
        .reverse
        .take(5)

      CardValue.getValue(allCardRank)
    }

    override def checkComb(listOfCards: List[Card]): Boolean = true
  }

  case object Pair extends Combination {
    val pairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 10000, List(Three, Three) -> 20000, List(Four, Four) -> 30000,
      List(Five, Five) -> 40000, List(Six, Six) -> 50000, List(Seven, Seven) -> 60000, List(Eight, Eight) -> 70000,
      List(Nine, Nine) -> 80000, List(Ten, Ten) -> 90000, List(Jack, Jack) -> 100000, List(Queen, Queen) -> 110000,
      List(King, King) -> 120000, List(Ace, Ace) -> 130000)

    val startCombValue: Int = 1000000

    override def checkComb(listOfCards: List[Card]): Boolean = listOfCards
      .groupBy(_.rank)
      .filter(_._2.length == 2)
      .toList
      .length == 1


    override def getCombStrength(listOfCards: List[Card]): Int = {
      val allCardsRanks: List[Rank] = listOfCards
        .map(_.rank)
        .sortBy(CardValue.cardRankValue)


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

      startCombValue + pairValue(pair) + CardValue.getValue(allCardRanksWithoutPair)
    }
  }

  case object TwoPair extends Combination {

    val firstPairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 1000, List(Three, Three) -> 2000,
      List(Four, Four) -> 3000, List(Five, Five) -> 4000, List(Six, Six) -> 5000, List(Seven, Seven) -> 6000,
      List(Eight, Eight) -> 7000, List(Nine, Nine) -> 8000, List(Ten, Ten) -> 9000, List(Jack, Jack) -> 10000,
      List(Queen, Queen) -> 11000, List(King, King) -> 12000, List(Ace, Ace) -> 13000)

    val secondPairValue: Map[List[Rank], Int] = Map(List(Two, Two) -> 50, List(Three, Three) -> 100, List(Four, Four) -> 150,
      List(Five, Five) -> 200, List(Six, Six) -> 250, List(Seven, Seven) -> 300, List(Eight, Eight) -> 350,
      List(Nine, Nine) -> 400, List(Ten, Ten) -> 450, List(Jack, Jack) -> 500, List(Queen, Queen) -> 550,
      List(King, King) -> 600, List(Ace, Ace) -> 650)

    def startCombValue: Int = 2000000

    override def checkComb(listOfCards: List[Card]): Boolean = {
      listOfCards
        .groupBy(_.rank)
        .filter(_._2.length == 2)
        .values
        .toList
        .length >= 2
    }

    override def getCombStrength(listOfCards: List[Card]): Int = {
      val allCardsRanks: List[Rank] = listOfCards
        .map(_.rank)
        .sortBy(CardValue.cardRankValue)

      val listOfPairRank: List[List[Rank]] = listOfCards
        .groupBy(_.rank)
        .filter(_._2.length == 2)
        .values
        .toList
        .map(_.map(_.rank))
        .sortBy(firstPairValue)

      if (listOfPairRank.length == 3) {
        val listStrongestTwoPairRank: List[List[Rank]] = listOfPairRank.drop(1)
        val topRankOfRest: List[Rank] = allCardsRanks.diff(listStrongestTwoPairRank.flatten).reverse.take(1)

        startCombValue +
          secondPairValue(listStrongestTwoPairRank.head) +
          firstPairValue(listStrongestTwoPairRank.last) +
          CardValue.getValue(topRankOfRest)
      } else {
        val topRankOfRest: List[Rank] = allCardsRanks.diff(listOfPairRank.flatten).reverse.take(1)

        startCombValue +
          secondPairValue(listOfPairRank.head) +
          firstPairValue(listOfPairRank.last) +
          CardValue.getValue(topRankOfRest)
      }
    }
  }

  case object ThreeOfKind extends Combination {

    val threeOfKind: Map[List[Rank], Int] = Map(
      List(Two, Two, Two) -> 1000,
      List(Three, Three, Three) -> 2000,
      List(Four, Four, Four) -> 3000,
      List(Five, Five, Five) -> 4000,
      List(Six, Six, Six) -> 5000,
      List(Seven, Seven, Seven) -> 6000,
      List(Eight, Eight, Eight) -> 7000,
      List(Nine, Nine, Nine) -> 8000,
      List(Ten, Ten, Ten) -> 9000,
      List(Jack, Jack, Jack) -> 10000,
      List(Queen, Queen, Queen) -> 11000,
      List(King, King, King) -> 12000,
      List(Ace, Ace, Ace) -> 13000
    )

    def startCombValue: Int = 3000000

    override def checkComb(listOfCards: List[Card]): Boolean =
      listOfCards
        .groupBy(_.rank)
        .exists(_._2.length == 3)

    override def getCombStrength(listOfCards: List[Card]): Int = {
      val cardRankList: List[Rank] = listOfCards
        .map(_.rank)
        .sortBy(CardValue.cardRankValue)

      val listOfTopThreeOfKindsRank: List[Rank] = listOfCards
        .groupBy(_.rank)
        .filter(_._2.length == 3)
        .values
        .map(_.map(_.rank))
        .toList
        .sortBy(threeOfKind)
        .reverse
        .take(1)
        .flatten

      val twoTopRankOfRest: List[Rank] = cardRankList
        .diff(listOfTopThreeOfKindsRank)
        .reverse
        .take(2)

      startCombValue +
        threeOfKind(listOfTopThreeOfKindsRank) +
        CardValue.getValue(twoTopRankOfRest)
    }
  }

  case object Straight extends Combination {

    val straight: Map[List[Rank], Int] = Map(
      List(Ace, Two, Three, Four, Five)   -> 1,
      List(Two, Three, Four, Five, Six)   -> 2,
      List(Three, Four, Five, Six, Seven) -> 3,
      List(Four, Five, Six, Seven, Eight) -> 4,
      List(Five, Six, Seven, Eight, Nine) -> 5,
      List(Six, Seven, Eight, Nine, Ten)  -> 6,
      List(Seven, Eight, Nine, Ten, Jack) -> 7,
      List(Eight, Nine, Ten, Jack, Queen) -> 8,
      List(Nine, Ten, Jack, Queen, King)  -> 9,
      List(Ten, Jack, Queen, King, Ace)   -> 10)

    def startCombValue: Int = 4000000

    override def checkComb(listOfCards: List[Card]): Boolean = {
      val listOfAllStraightComb: List[List[Rank]] = straight.keySet.toList
      val cardRank: List[Rank] = listOfCards.map(_.rank).sortBy(CardValue.cardRankValue)

      listOfAllStraightComb.map(_.diff(cardRank)).exists(_.isEmpty)
    }

    override def getCombStrength(listOfCards: List[Card]): Int = {
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

  case object Flush extends Combination {
    def startCombValue: Int = 5000000

    override def checkComb(listOfCards: List[Card]): Boolean = {
      listOfCards
        .groupBy(_.suit)
        .exists(_._2.length >= 5)
    }

    override def getCombStrength(listOfCards: List[Card]): Int = {
      val topFiveRankCardSimilarSuits: List[Rank] = listOfCards
        .groupBy(_.suit)
        .filter(_._2.length >= 5)
        .flatMap(_._2.map(_.rank))
        .toList
        .sortBy(CardValue.cardRankValue)
        .reverse
        .take(5)

      startCombValue + CardValue.getValue(topFiveRankCardSimilarSuits)
    }
  }

  case object FullHouse extends Combination {

    val threeOfKind: Map[List[Rank], Int] = Map(
      List(Two, Two, Two)       -> 100,
      List(Three, Three, Three) -> 200,
      List(Four, Four, Four)    -> 300,
      List(Five, Five, Five)    -> 400,
      List(Six, Six, Six)       -> 500,
      List(Seven, Seven, Seven) -> 600,
      List(Eight, Eight, Eight) -> 700,
      List(Nine, Nine, Nine)    -> 800,
      List(Ten, Ten, Ten)       -> 900,
      List(Jack, Jack, Jack)    -> 1000,
      List(Queen, Queen, Queen) -> 1100,
      List(King, King, King)    -> 1200,
      List(Ace, Ace, Ace)       -> 1300)

    val pair: Map[List[Rank], Int] = Map(
      List(Two, Two)     -> 1,
      List(Three, Three) -> 2,
      List(Four, Four)   -> 3,
      List(Five, Five)   -> 4,
      List(Six, Six)     -> 5,
      List(Seven, Seven) -> 6,
      List(Eight, Eight) -> 7,
      List(Nine, Nine)   -> 8,
      List(Ten, Ten)     -> 9,
      List(Jack, Jack)   -> 10,
      List(Queen, Queen) -> 11,
      List(King, King)   -> 12,
      List(Ace, Ace)     -> 13)

    def startCombValue: Int = 6000000

    override def checkComb(listOfCards: List[Card]): Boolean = {
      val mapRankListCardWithSimilarRank: Map[Rank, List[Card]] = listOfCards.groupBy(_.rank)
      val numberOfTreeOfKind: Int = mapRankListCardWithSimilarRank.count(_._2.length == 3)
      val numberOfPair: Int = mapRankListCardWithSimilarRank.count(_._2.length == 2)

      numberOfTreeOfKind == 2 || (numberOfTreeOfKind == 1 && numberOfPair >= 1)
    }

    override def getCombStrength(listOfCards: List[Card]): Int = {
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

  case object FourOfKind extends Combination {

    val fourOfAKind: Map[List[Rank], Int] = Map(
      List(Two, Two, Two, Two)         -> 100,
      List(Three, Three, Three, Three) -> 200,
      List(Four, Four, Four, Four)     -> 300,
      List(Five, Five, Five, Five)     -> 400,
      List(Six, Six, Six, Six)         -> 500,
      List(Seven, Seven, Seven, Seven) -> 600,
      List(Eight, Eight, Eight, Eight) -> 700,
      List(Nine, Nine, Nine, Nine)     -> 800,
      List(Ten, Ten, Ten, Ten)         -> 900,
      List(Jack, Jack, Jack, Jack)     -> 1000,
      List(Queen, Queen, Queen, Queen) -> 1100,
      List(King, King, King, King)     -> 1200,
      List(Ace, Ace, Ace, Ace)         -> 1300)

    def startCombValue: Int = 7000000

    override def checkComb(listOfCards: List[Card]): Boolean =
      listOfCards
        .groupBy(_.rank)
        .exists(_._2.length == 4)


    override def getCombStrength(listOfCards: List[Card]): Int = {
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
          .sortBy(CardValue.cardRankValue)
          .reverse
          .take(1)

      startCombValue + fourOfAKind(rankListFourOfKind) + CardValue.getValue(restRankOfCards)
    }
  }

  case object StraightFlush extends Combination {

    val straight: Map[List[Rank], Int] = Map(
      List(Ace, Two, Three, Four, Five)   -> 1,
      List(Two, Three, Four, Five, Six)   -> 2,
      List(Three, Four, Five, Six, Seven) -> 3,
      List(Four, Five, Six, Seven, Eight) -> 4,
      List(Five, Six, Seven, Eight, Nine) -> 5,
      List(Six, Seven, Eight, Nine, Ten)  -> 6,
      List(Seven, Eight, Nine, Ten, Jack) -> 7,
      List(Eight, Nine, Ten, Jack, Queen) -> 8,
      List(Nine, Ten, Jack, Queen, King)  -> 9,
      List(Ten, Jack, Queen, King, Ace)   -> 10)

    def startCombValue: Int = 8000000

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

    override def getCombStrength(listOfCards: List[Card]): Int = {
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
}
