package com.mypoker

import com.mypoker.domain.Card
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._

object Myyyy extends App {

  val getStrength = CalculateCombinationStrength()

  val flush = List(Card(Four, Clubs), Card(King, Spades), Card(Four, Hearts),
    Card(Eight, Spades), Card(Seven, Spades), Card(Ace, Spades), Card(Nine, Spades))

  val fullHouse = List(Card(Four, Clubs), Card(King, Spades), Card(Four, Hearts),
    Card(Eight, Spades), Card(Seven, Spades), Card(King, Hearts), Card(King, Diamonds))

  val straightFlush = List(Card(Nine, Spades), Card(Five, Spades), Card(Eight, Spades),
    Card(Eight, Spades), Card(Seven, Spades), Card(King, Hearts), Card(Six, Spades))

  println(getStrength(flush))
  println(getStrength(fullHouse))
  println(getStrength(straightFlush))

}
