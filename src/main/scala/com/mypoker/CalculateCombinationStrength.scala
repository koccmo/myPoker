package com.mypoker

import com.mypoker.domain.Rank._
import com.mypoker.domain.{Card, Rank}

trait CalculateCombinationStrength {
  def apply(cards: List[Card]): Int
}

object CalculateCombinationStrength {

  private val calculateCardsStrength: CalculateCardsStrength = CalculateCardsStrength()

  private val DefaultStraightFlush: Int    = 8000000
  private val DefaultFourOfKind: Int       = 7000000
  private val DefaultFullHouse: Int        = 6000000
  private val DefaultFlushValue: Int       = 5000000
  private val DefaultStraightValue: Int    = 4000000
  private val DefaultThreeOfKindValue: Int = 3000000
  private val DefaultTwoPairValue: Int     = 2000000
  private val DefaultPairValue: Int        = 1000000

  private val PairValueMap: Map[List[Rank], Int] = Map(
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

  def apply(): CalculateCombinationStrength =
    new CalculateCombinationStrength {

      def apply(cards: List[Card]): Int =
        if (straightFlush(cards)) getStraightFlushStrength(cards)
        else if (fourOfKind(cards)) getFourOfKindStrength(cards)
        else if (fullHouse(cards)) getFullHouseStrength(cards)
        else if (flush(cards)) getFlushStrength(cards)
        else if (straight(cards)) getStraightStrength(cards)
        else if (threeOfKind(cards)) getThreeOfKindStrength(cards)
        else if (twoPair(cards)) getTwoPairStrength(cards)
        else if (pair(cards)) getPairStrength(cards)
        else getHighCardStrength(cards)

      private def straightFlush(cards: List[Card]): Boolean = {
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

      private def fourOfKind(cards: List[Card]): Boolean =
        cards
          .groupBy(_.rank)
          .exists { case (_, cards) => cards.length == 4 }

      private def fullHouse(cards: List[Card]): Boolean = {
        val map: Map[Rank, List[Card]] = cards.groupBy(_.rank)
        val treeOfKinds: Int           = map.count { case (_, cards) => cards.length == 3 }
        val pairs: Int                 = map.count { case (_, cards) => cards.length == 2 }

        treeOfKinds == 2 || (treeOfKinds == 1 && pairs >= 1)
      }

      private def flush(cards: List[Card]): Boolean =
        cards
          .groupBy(_.suit)
          .exists { case (_, cards) => cards.length >= 5 }

      private def straight(cards: List[Card]): Boolean = {
        val combinationRanks: List[List[Rank]] = StraightMap.keySet.toList
        val cardRank: List[Rank]               = cards.map(_.rank).sortBy(CalculateCardsStrength.cardRankValue)

        combinationRanks.map(_.diff(cardRank)).exists(_.isEmpty)
      }

      private def threeOfKind(cards: List[Card]): Boolean =
        cards
          .groupBy(_.rank)
          .exists { case (_, cards) => cards.length == 3 }

      private def twoPair(cards: List[Card]): Boolean     =
        cards
          .groupBy(_.rank)
          .filter { case (_, cards) => cards.length == 2 }
          .values
          .toList
          .length >= 2

      private def pair(cards: List[Card]): Boolean =
        cards
          .groupBy(_.rank)
          .filter { case (_, cards) => cards.length == 2 }
          .toList
          .length == 1

      private def getStraightFlushStrength(cards: List[Card]): Int = {
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

        DefaultStraightFlush + combinationValue
      }

      private def getFourOfKindStrength(cards: List[Card]): Int = {
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
            .sortBy(CalculateCardsStrength.cardRankValue)
            .reverse
            .take(1)

        DefaultFourOfKind + FourOfKindMap.getOrElse(fourOfKind, 0) + calculateCardsStrength(otherCards)
      }

      private def getFullHouseStrength(cards: List[Card]): Int = {
        val threeOfKinds: List[List[Rank]] = cards
          .groupBy(_.rank)
          .filter { case (_, cards) => cards.length == 3 }
          .values
          .toList
          .map(_.map(_.rank))

        if (threeOfKinds.length == 1) {
          val strongestPair: List[List[Rank]] = cards
            .groupBy(_.rank)
            .filter { case (_, cards) => cards.length == 2 }
            .values
            .toList
            .map(_.map(_.rank))
            .sortBy(SecondPairMap)
            .reverse
            .take(1)

          DefaultFullHouse + ThreeOfKindMap.getOrElse(threeOfKinds.head, 0) +
            SecondPairMap.getOrElse(strongestPair.head, 0)
        } else {
          val strongestThreeOfKind: List[Rank] =
            threeOfKinds
              .maxBy(ThreeOfKindMap)

          val pair: List[Rank] =
            threeOfKinds
              .minBy(ThreeOfKindMap)
              .take(2)

          DefaultFullHouse + ThreeOfKindMap.getOrElse(strongestThreeOfKind, 0) +
            SecondPairMap.getOrElse(pair, 0)
        }
      }

      private def getFlushStrength(cards: List[Card]): Int = {
        val topFiveRankCardSimilarSuits: List[Rank] = cards
          .groupBy(_.suit)
          .filter { case (_, cards) => cards.length >= 5 }
          .flatMap { case (_, cards) => cards.map(_.rank) }
          .toList
          .sortBy(CalculateCardsStrength.cardRankValue)
          .reverse
          .take(5)

        DefaultFlushValue + calculateCardsStrength(topFiveRankCardSimilarSuits)
      }

      private def getStraightStrength(cards: List[Card]): Int = {
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

      private def getThreeOfKindStrength(cards: List[Card]): Int = {
        val ranks: List[Rank] = cards
          .map(_.rank)
          .sortBy(CalculateCardsStrength.cardRankValue)

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
          calculateCardsStrength(otherTwoHighCards)
      }

      private def getTwoPairStrength(cards: List[Card]): Int = {
        val ranks: List[Rank] = cards
          .map(_.rank)
          .sortBy(CalculateCardsStrength.cardRankValue)

        val pairs: List[List[Rank]] = cards
          .groupBy(_.rank)
          .filter { case (_, cards) => cards.length == 2 }
          .values
          .toList
          .map(_.map(_.rank))
          .sortBy(PairValueMap)

        if (pairs.length == 3) {
          val strongestTwoPair: List[List[Rank]] = pairs.drop(1)
          val highestRank: List[Rank]          = ranks.diff(strongestTwoPair.flatten).reverse.take(1)

          DefaultTwoPairValue +
            SecondPairMap.getOrElse(strongestTwoPair.head, 0) +
            PairValueMap.getOrElse(strongestTwoPair.last, 0) +
            calculateCardsStrength(highestRank)
        } else {
          val highestRank: List[Rank] = ranks.diff(pairs.flatten).reverse.take(1)

          DefaultTwoPairValue +
            SecondPairMap.getOrElse(pairs.head, 0) +
            PairValueMap.getOrElse(pairs.last, 0) +
            calculateCardsStrength(highestRank)
        }
      }

      private def getPairStrength(cards: List[Card]): Int = {
        val ranks: List[Rank] = cards
          .map(_.rank)
          .sortBy(CalculateCardsStrength.cardRankValue)

        val pair: List[Rank] = cards
          .groupBy(_.rank)
          .filter{ case (_, cards) => cards.length == 2 }
          .flatMap{ case (_, cards) => cards }
          .map(_.rank)
          .toList

        val otherCards: List[Rank] = ranks
          .diff(pair)
          .drop(2)
          .reverse

        DefaultPairValue + PairValueMap.getOrElse(pair, 0) + calculateCardsStrength(otherCards)
      }

      private def getHighCardStrength(cards: List[Card]): Int = {
        val ranks: List[Rank] = cards
          .map(card => card.rank)
          .sortBy(CalculateCardsStrength.cardRankValue)
          .reverse
          .take(5)

        calculateCardsStrength(ranks)
      }
    }
}
