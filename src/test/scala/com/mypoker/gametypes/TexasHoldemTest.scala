package com.mypoker.gametypes

import com.mypoker.domain.Card
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTest extends AnyFunSuite with Matchers{



  test("Get Value of Card test") {
    val listOfCards: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades), Card(Queen, Hearts), Card(Five, Hearts))

//    val result: Int = getStrength(listOfCards)
//
//    result shouldBe 6001112
  }

//  test("Create answer from Tuple String -> Int TEST") {
//    val board: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
//      Card(Eight, Spades)))
//
//    val hands: List[Hand] = List(
//      Hand(List(Card(Queen, Spades), Card(Queen, Hearts))),
//      Hand(List(Card(Six, Hearts), Card(Six, Hearts))),
//      Hand(List(Card(Queen, Hearts), Card(Queen, Hearts))))
//
//
//    val result = TexasHoldem.getAnswer(board, hands)
//
//    result shouldBe "6h6h QhQh=QsQh"
//  }
}
