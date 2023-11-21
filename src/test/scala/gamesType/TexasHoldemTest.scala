package gamesType

import cards.{Board, Card, Hand}
import cards.Rank.{Eight, Five, King, Nine, Queen, Six, Ten}
import cards.Suit.{Hearts, Spades}
import combinations.Combination
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTest extends AnyFunSuite with Matchers{


  test("Create Tuple (String -> Int)  from list of Cards and same list in string format ") {
    val boards: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades)))

    val hands: List[Hand] = List(Hand(List(Card(Queen, Hearts), Card(Queen, Hearts))), Hand(List(Card(Six, Hearts), Card(Six, Hearts))))
    val listHands: List[String] = List("QhQh", "6h6h")

    val listOTuple: List[(String, List[Card])] = TexasHoldem.createListOfTupleHandListOfCards(boards, hands, listHands)
    val result = listOTuple.map { case (string, cards) => (string, Combination.getCardValue(cards)) }
    result shouldBe List(("QhQh", 7001112), ("6h6h", 2012557))
  }

  test("Get Value of Card test") {
    val listOfCards: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades), Card(Queen, Hearts), Card(Five, Hearts))

    val result: Int = Combination.getCardValue(listOfCards)

    result shouldBe 6001112
  }

  test("Create answer from Tuple String -> Int TEST") {
    val board: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades)))

    val hands: List[Hand] = List(Hand(List(Card(Queen, Spades), Card(Queen, Hearts))), Hand(List(Card(Six, Hearts), Card(Six, Hearts))), Hand(List(Card(Queen, Hearts), Card(Queen, Hearts))))
    val listHands: List[String] = List("QsQh", "6h6h", "QhQh")

    val listOfTuple = TexasHoldem.createListOfTupleHandListOfCards(board, hands, listHands)

    val result: String = TexasHoldem.createAnswerFromTuple(listOfTuple.map { case (string, cards) => (string, Combination.getCardValue(cards)) })

    result shouldBe "6h6h QhQh=QsQh"
  }
}
