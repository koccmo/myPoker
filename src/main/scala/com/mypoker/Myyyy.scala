package com.mypoker

import com.mypoker.domain.{Card, Rank}
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._

import scala.annotation.tailrec

object Myyyy extends App {

  val cardRankValue: Map[Rank, Int] = Map(
    Two   -> 1,
    Three -> 2,
    Four  -> 3,
    Five  -> 4,
    Six   -> 5,
    Seven -> 6,
    Eight -> 7,
    Nine  -> 8,
    Ten   -> 9,
    Jack  -> 10,
    Queen -> 11,
    King  -> 12,
    Ace   -> 13
  )

  val getStrength = CalculateCombinationStrength()

  val flush = List(
    Card(Four, Clubs),
    Card(King, Spades),
    Card(Four, Hearts),
    Card(Eight, Spades),
    Card(Seven, Spades),
    Card(Ace, Spades),
    Card(Nine, Spades)
  )

  val fullHouse = List(
    Card(Four, Clubs),
    Card(King, Spades),
    Card(Four, Hearts),
    Card(Eight, Spades),
    Card(Seven, Spades),
    Card(King, Hearts),
    Card(King, Diamonds)
  )

  val straightFlush = List(
    Card(Nine, Spades),
    Card(Five, Spades),
    Card(Eight, Spades),
    Card(Eight, Spades),
    Card(Seven, Spades),
    Card(King, Hearts),
    Card(Six, Spades)
  )

  def getCardsStrength(ranks: List[Rank]): Int = {
    @tailrec
    def helper(ranks: List[Rank], rankValue: Int): Int = {
      val newRankValue = ranks.length match {
        case length if 2 to 5 contains length =>
          Math.pow(13, ranks.length - 1) * cardRankValue.getOrElse(ranks.head, 0) + rankValue
        case 1                                      => cardRankValue.getOrElse(ranks.head, 0) + rankValue
        case _                                      => rankValue
      }

      ranks match {
        case _ :: tail => helper(tail, newRankValue.toInt)
        case _         => newRankValue.toInt
      }
    }

    helper(ranks, 0)
  }

  val threeCards: List[Rank] = List(Two, Five, Six).reverse

  println(getStrength(flush))
  println(getStrength(fullHouse))
  println(getStrength(straightFlush))
  println(getCardsStrength(threeCards))

}
