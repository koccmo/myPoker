package com.mypoker.services

import com.mypoker.domain.Rank._
import com.mypoker.domain.{Card, Rank, Suit}

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

  private val StraightCombinations: List[List[Rank]] =
    List(
      List(Ace, Two, Three, Four, Five),
      List(Two, Three, Four, Five, Six),
      List(Three, Four, Five, Six, Seven),
      List(Four, Five, Six, Seven, Eight),
      List(Five, Six, Seven, Eight, Nine),
      List(Six, Seven, Eight, Nine, Ten),
      List(Seven, Eight, Nine, Ten, Jack),
      List(Eight, Nine, Ten, Jack, Queen),
      List(Nine, Ten, Jack, Queen, King),
      List(Ten, Jack, Queen, King, Ace)
    )

  def apply(): CalculateStrength =
    new CalculateStrength {

      def apply(cards: List[Card]): Int = {

        val rankCardsTuples: List[(Rank, List[Card])] = cards.groupBy(_.rank).toList
        val suitCardsTuples: List[(Suit, List[Card])] = cards.groupBy(_.suit).toList

        def straightFlush: Boolean = {
          val fivePlusRank: List[Rank] = suitCardsTuples
              .filter { case (_, cards) => cards.length >= 5 }
              .map { case (_, cards) => cards }
              .flatMap(_.map(_.rank))

          StraightCombinations
            .map(_.intersect(fivePlusRank))
            .exists(_.length == 5)
        }

        def fourOfKind: Boolean = rankCardsTuples
            .exists { case (_, cards) => cards.length == 4 }

        def fullHouse: Boolean = {
          val treeOfKinds: Int = rankCardsTuples.count { case (_, cards) => cards.length == 3 }
          val pairs: Int       = rankCardsTuples.count { case (_, cards) => cards.length == 2 }

          treeOfKinds == 2 || (treeOfKinds == 1 && pairs >= 1)
        }

        def flush: Boolean = suitCardsTuples
            .exists { case (_, cards) => cards.length >= 5 }

        def straight: Boolean = {
          val cardRank: List[Rank] = cards.map(_.rank)

          StraightCombinations.map(_.diff(cardRank)).exists(_.isEmpty)
        }

        def threeOfKind: Boolean = rankCardsTuples
            .exists { case (_, cards) => cards.length == 3 }

        def twoPair: Boolean = rankCardsTuples
            .filter { case (_, cards) => cards.length == 2 }
            .map { case (_, cards) => cards }
            .length >= 2

        def pair: Boolean =
          rankCardsTuples.count { case (_, cards) => cards.length == 2 } == 1

        def getStraightFlushStrength: Int = {
          val ranksWithSimilarSuit: List[List[Rank]] = suitCardsTuples
              .filter { case (_, cards) => cards.length >= 5 }
              .map { case (_, cards)    => cards }
              .map(_.map(_.rank))

          DefaultStraightFlushValue + ranksWithSimilarSuit.map(getRanksStrength).max
        }

        def getFourOfKindStrength: Int = {
          val fourOfKind: List[Rank] = rankCardsTuples
              .filter { case (_, cards) => cards.length == 4 }
              .map { case (_, cards) => cards }
              .flatMap(_.map(_.rank))

          val otherCards: List[Rank] = cards
              .map(_.rank)
              .diff(fourOfKind)
              .sortBy(_.strength)
              .reverse
              .take(1)

          DefaultFourOfKindValue + getRanksStrength(fourOfKind ++ otherCards)
        }

        def getFullHouseStrength: Int = {
          val threeOfKind: List[Rank] = cards
            .map(_.rank)
            .groupBy(_.strength)
            .filter { case (_, ranks) => ranks.length == 3 }
            .toList
            .sortBy { case (value, _) => value }
            .reverse
            .take(1)
            .flatMap { case (_, ranks) => ranks }

          val pair: List[Rank]        = cards
            .map(_.rank)
            .diff(threeOfKind)
            .groupBy(_.strength)
            .filter { case (_, ranks) => ranks.length >= 2 }
            .toList
            .reverse
            .take(1)
            .flatMap { case (_, ranks) => ranks.take(2) }

          DefaultFullHouseValue + getRanksStrength(threeOfKind ++ pair)
        }

        def getFlushStrength: Int = {
          val topFiveRankCardSimilarSuits: List[Rank] = suitCardsTuples
            .filter { case (_, cards) => cards.length >= 5 }
            .flatMap { case (_, cards) => cards.map(_.rank) }
            .sortBy(_.strength)
            .reverse
            .take(5)

          DefaultFlushValue + getRanksStrength(topFiveRankCardSimilarSuits)
        }

        def getStraightStrength: Int = {
          val straights: List[List[Rank]] = StraightCombinations
            .map(_.intersect(cards.map(_.rank)))

          DefaultStraightValue + straights.map(getRanksStrength).max
        }

        def getThreeOfKindStrength: Int = {
          val threeOfKind: List[Rank]     = cards
            .map(_.rank)
            .groupBy(_.strength)
            .filter { case (_, ranks) => ranks.length == 3 }
            .toList
            .sortBy { case (value, _) => value }
            .reverse
            .take(1)
            .flatMap { case (_, ranks) => ranks }

          val twoHighestRanks: List[Rank] = cards
            .map(_.rank)
            .diff(threeOfKind)
            .sortBy(_.strength)
            .reverse
            .take(2)

          DefaultThreeOfKindValue + getRanksStrength(threeOfKind ++ twoHighestRanks)
        }

        def getTwoPairStrength: Int = {
          val pairs: List[Rank] = cards
            .map(_.rank)
            .groupBy(_.strength)
            .filter { case (_, ranks) => ranks.length == 2 }
            .flatMap { case (_, ranks) => ranks }
            .toList
            .sortBy(_.strength)
            .reverse
            .take(4)

          val highestRank: List[Rank] = cards
            .map(_.rank)
            .diff(pairs)
            .sortBy(_.strength)
            .reverse
            .take(1)

          DefaultTwoPairValue + getRanksStrength(pairs ++ highestRank)
        }

        def getPairStrength: Int = {
          val pair = cards
            .map(_.rank)
            .groupBy(_.strength)
            .filter { case (_, ranks) => ranks.length == 2 }
            .flatMap { case (_, ranks) => ranks }
            .toList

          val threeHighestRanks: List[Rank] = cards
            .map(_.rank)
            .diff(pair)
            .sortBy(_.strength)
            .reverse
            .take(3)

          DefaultPairValue + getRanksStrength(pair ++ threeHighestRanks)
        }

        def getHighCardStrength: Int = {
          val ranks: List[Rank] = cards
            .map(card => card.rank)
            .sortBy(_.strength)
            .reverse
            .take(5)

          getRanksStrength(ranks)
        }

        def getRanksStrength(ranks: List[Rank]): Int = {
          val (result, _) = ranks.foldLeft(0, ranks.length) {
            case ((result, length), rank) => (Math.pow(rank.strength, length).toInt + result, length - 1)
          }

          result
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
