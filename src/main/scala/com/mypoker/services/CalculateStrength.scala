package com.mypoker.services

import com.mypoker.domain.Rank._
import com.mypoker.domain.{Card, Rank}

trait CalculateStrength {

  def apply(cards: List[Card]): Int
}

object CalculateStrength {

  private val DefaultStraightFlushValue: Int = 8000000
  private val DefaultFourOfKindValue: Int    = 7000000
  private val DefaultFullHouseValue: Int     = 6000000
  private val DefaultFlushValue: Int         = 5000000
  private val DefaultStraightValue: Int      = 4000000
  private val DefaultThreeOfKindValue: Int   = 3000000
  private val DefaultTwoPairValue: Int       = 2000000
  private val DefaultPairValue: Int          = 1000000

  private val PairMap: Map[List[Rank], Int] = Map(
    List(Two, Two)     -> 10000,
    List(Three, Three) -> 20000,
    List(Four, Four)   -> 30000,
    List(Five, Five)   -> 40000,
    List(Six, Six)     -> 50000,
    List(Seven, Seven) -> 60000,
    List(Eight, Eight) -> 70000,
    List(Nine, Nine)   -> 80000,
    List(Ten, Ten)     -> 90000,
    List(Jack, Jack)   -> 100000,
    List(Queen, Queen) -> 110000,
    List(King, King)   -> 120000,
    List(Ace, Ace)     -> 130000
  )

  private val SecondPairMap: Map[List[Rank], Int] = Map(
    List(Two, Two)     -> 50,
    List(Three, Three) -> 100,
    List(Four, Four)   -> 150,
    List(Five, Five)   -> 200,
    List(Six, Six)     -> 250,
    List(Seven, Seven) -> 300,
    List(Eight, Eight) -> 350,
    List(Nine, Nine)   -> 400,
    List(Ten, Ten)     -> 450,
    List(Jack, Jack)   -> 500,
    List(Queen, Queen) -> 550,
    List(King, King)   -> 600,
    List(Ace, Ace)     -> 650
  )

  private val ThreeOfKindMap: Map[List[Rank], Int] = Map(
    List(Two, Two, Two)       -> 1000,
    List(Three, Three, Three) -> 2000,
    List(Four, Four, Four)    -> 3000,
    List(Five, Five, Five)    -> 4000,
    List(Six, Six, Six)       -> 5000,
    List(Seven, Seven, Seven) -> 6000,
    List(Eight, Eight, Eight) -> 7000,
    List(Nine, Nine, Nine)    -> 8000,
    List(Ten, Ten, Ten)       -> 9000,
    List(Jack, Jack, Jack)    -> 10000,
    List(Queen, Queen, Queen) -> 11000,
    List(King, King, King)    -> 12000,
    List(Ace, Ace, Ace)       -> 13000
  )

  private val StraightMap: Map[List[Rank], Int] = Map(
    List(Ace, Two, Three, Four, Five)   -> 1,
    List(Two, Three, Four, Five, Six)   -> 2,
    List(Three, Four, Five, Six, Seven) -> 3,
    List(Four, Five, Six, Seven, Eight) -> 4,
    List(Five, Six, Seven, Eight, Nine) -> 5,
    List(Six, Seven, Eight, Nine, Ten)  -> 6,
    List(Seven, Eight, Nine, Ten, Jack) -> 7,
    List(Eight, Nine, Ten, Jack, Queen) -> 8,
    List(Nine, Ten, Jack, Queen, King)  -> 9,
    List(Ten, Jack, Queen, King, Ace)   -> 10
  )

  private val FourOfKindMap: Map[List[Rank], Int] = Map(
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
    List(Ace, Ace, Ace, Ace)         -> 1300
  )

  private def getRanksStrength(ranks: List[Rank]): Int = {
    val (result, _) = ranks.foldLeft(0, ranks.length) {
      case ((result, length), rank) => (Math.pow(rank.strength, length).toInt + result, length - 1)
    }

    result
  }

  def apply(): CalculateStrength =

    new CalculateStrength {

      def apply(cards: List[Card]): Int = {

        def straightFlush: Boolean = {
          val fivePlusRank: List[Rank] =
            cards
              .groupBy(_.suit)
              .filter { case (_, cards) => cards.length >= 5 }
              .values
              .flatMap(_.map(_.rank))
              .toList

          StraightMap.keys.toList
            .map(_.intersect(fivePlusRank))
            .exists(_.length == 5)
        }

        def fourOfKind: Boolean =
          cards
            .groupBy(_.rank)
            .exists { case (_, cards) => cards.length == 4 }

        def fullHouse: Boolean = {
          val map: Map[Rank, List[Card]] = cards.groupBy(_.rank)
          val treeOfKinds: Int = map.count { case (_, cards) => cards.length == 3 }
          val pairs: Int = map.count { case (_, cards) => cards.length == 2 }

          treeOfKinds == 2 || (treeOfKinds == 1 && pairs >= 1)
        }

        def flush: Boolean =
          cards
            .groupBy(_.suit)
            .exists { case (_, cards) => cards.length >= 5 }

        def straight: Boolean = {
          val combinationRanks: List[List[Rank]] = StraightMap.keySet.toList
          val cardRank: List[Rank] = cards.map(_.rank).sortBy(_.strength)

          combinationRanks.map(_.diff(cardRank)).exists(_.isEmpty)
        }

        def threeOfKind: Boolean =
          cards
            .groupBy(_.rank)
            .exists { case (_, cards) => cards.length == 3 }

        def twoPair: Boolean =
          cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 2 }
            .values
            .toList
            .length >= 2

        def pair: Boolean =
          cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 2 }
            .toList
            .length == 1

        def getStraightFlushStrength: Int = {
          val ranksWithSimilarSuit: List[Rank] =
            cards
              .groupBy(_.suit)
              .filter { case (_, cards) => cards.length >= 5 }
              .values
              .flatMap(_.map(_.rank))
              .toList

          val combinationValue: Int =
            StraightMap.keys
              .map(_.intersect(ranksWithSimilarSuit))
              .filter(_.length == 5)
              .map(ranks => StraightMap.getOrElse(ranks, 0))
              .max

          DefaultStraightFlushValue + combinationValue
        }

        def getFourOfKindStrength: Int = {
          val fourOfKind: List[Rank] =
            cards
              .groupBy(_.rank)
              .filter { case (_, cards) => cards.length == 4 }
              .values
              .toList
              .flatMap(_.map(_.rank))

          val otherCards: List[Rank] =
            cards
              .map(_.rank)
              .diff(fourOfKind)
              .sortBy(_.strength)
              .reverse
              .take(1)

          DefaultFourOfKindValue + FourOfKindMap.getOrElse(fourOfKind, 0) + getRanksStrength(otherCards)
        }

        def getFullHouseStrength: Int = {
          val threeOfKinds: List[List[Rank]] = cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 3 }
            .values
            .toList
            .map(_.map(_.rank))

          if (threeOfKinds.length == 1) {
            val strongestPair: List[Rank] = cards
              .groupBy(_.rank)
              .filter { case (_, cards) => cards.length == 2 }
              .values
              .toList
              .map(_.map(_.rank))
              .sortBy(SecondPairMap)
              .reverse
              .take(1)
              .flatten

            DefaultFullHouseValue + ThreeOfKindMap.getOrElse(threeOfKinds.flatten, 0) +
              SecondPairMap.getOrElse(strongestPair, 0)
          } else {
            val strongestThreeOfKind: List[Rank] =
              threeOfKinds
                .maxBy(ThreeOfKindMap)

            val pair: List[Rank] =
              threeOfKinds
                .minBy(ThreeOfKindMap)
                .take(2)

            DefaultFullHouseValue + ThreeOfKindMap.getOrElse(strongestThreeOfKind, 0) +
              SecondPairMap.getOrElse(pair, 0)
          }
        }

        def getFlushStrength: Int = {
          val topFiveRankCardSimilarSuits: List[Rank] = cards
            .groupBy(_.suit)
            .filter { case (_, cards) => cards.length >= 5 }
            .flatMap { case (_, cards) => cards.map(_.rank) }
            .toList
            .sortBy(_.strength)
            .reverse
            .take(5)

          DefaultFlushValue + getRanksStrength(topFiveRankCardSimilarSuits)
        }

        def getStraightStrength: Int = {
          val straightCombination: List[List[Rank]] = StraightMap.keys.toList

          val cardRanks: List[Rank] = cards.map(_.rank)

          val foundedCombinations: List[List[Rank]] = straightCombination
            .map(_.intersect(cardRanks))
            .filter(_.length == 5)

          val valueTopCombination: Int = foundedCombinations
            .map(combination => StraightMap(combination))
            .max

          DefaultStraightValue + valueTopCombination
        }

        def getThreeOfKindStrength: Int = {
          val ranks: List[Rank] = cards
            .map(_.rank)
            .sortBy(_.strength)

          val threeOfKinds: List[Rank] = cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 3 }
            .values
            .map(_.map(_.rank))
            .toList
            .sortBy(ThreeOfKindMap)
            .reverse
            .take(1)
            .flatten

          val otherTwoHighCards: List[Rank] = ranks
            .diff(threeOfKinds)
            .reverse
            .take(2)

          DefaultThreeOfKindValue +
            ThreeOfKindMap.getOrElse(threeOfKinds, 0) +
            getRanksStrength(otherTwoHighCards)
        }

        def getTwoPairStrength: Int = {
          val ranks: List[Rank] = cards
            .map(_.rank)
            .sortBy(_.strength)

          val pairs: List[List[Rank]] = cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 2 }
            .values
            .toList
            .map(_.map(_.rank))
            .sortBy(PairMap)

          val strongPair: List[Rank] = pairs
            .reverse
            .take(1)
            .flatten

          val weakPair: List[Rank] = pairs
            .reverse
            .slice(1, 2)
            .flatten

          val highestRank: List[Rank] = ranks
            .diff(strongPair ++ weakPair)
            .reverse
            .take(1)

            DefaultTwoPairValue +
              SecondPairMap.getOrElse(weakPair, 0) +
              PairMap.getOrElse(strongPair, 0) +
              getRanksStrength(highestRank)
        }

        def getPairStrength: Int = {
          val ranks: List[Rank] = cards
            .map(_.rank)
            .sortBy(_.strength)

          val pair: List[Rank] = cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 2 }
            .flatMap { case (_, cards) => cards }
            .map(_.rank)
            .toList

          val otherCards: List[Rank] = ranks
            .diff(pair)
            .drop(2)
            .reverse

          DefaultPairValue + PairMap.getOrElse(pair, 0) + getRanksStrength(otherCards)
        }

        def getHighCardStrength: Int = {
          val ranks: List[Rank] = cards
            .map(card => card.rank)
            .sortBy(_.strength)
            .reverse
            .take(5)

          getRanksStrength(ranks)
        }

        if (straightFlush) getStraightFlushStrength
        else if (fourOfKind) getFourOfKindStrength
        else if (fullHouse) getFullHouseStrength
        else if (flush) getFlushStrength
        else if (straight) getStraightStrength
        else if (threeOfKind) getThreeOfKindStrength
        else if (twoPair) getTwoPairStrength
        else if (pair) getPairStrength
        else getHighCardStrength
      }
    }
}
