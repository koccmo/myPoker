package gamesType

import cards.{Board, Card, Hand}
import cards.Rank.{Eight, Five, King, Queen, Six}
import cards.Suit.{Hearts, Spades}
import combinations.Combination
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTest extends AnyFunSuite with Matchers{


  test("Create Tuple (Hand -> Int)  from list of Cards and same list in string format ") {
    val boards: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades)))

    val hands: List[Hand] = List(Hand(List(Card(Queen, Hearts), Card(Queen, Hearts))), Hand(List(Card(Six, Hearts), Card(Six, Hearts))))

    val result: List[(Hand, Int)] = TexasHoldem.createTuples(boards, hands)

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

    val hands: List[Hand] = List(
      Hand(List(Card(Queen, Spades), Card(Queen, Hearts))),
      Hand(List(Card(Six, Hearts), Card(Six, Hearts))),
      Hand(List(Card(Queen, Hearts), Card(Queen, Hearts))))

    val listOfTuple = TexasHoldem.createTuples(board, hands)

    val result = TexasHoldem.createAnswer(listOfTuple)

    result shouldBe "6h6h QhQh=QsQh"
  }
}
