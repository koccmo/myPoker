package games

import cards.{Board, Card, Hands}
import cards.Rank.{Eight, Five, King, Nine, Queen, Six, Ten}
import cards.Suit.{Hearts, Spades}
import combinations.{Combination, Flush, FourOfKind, FullHouse, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTest extends AnyFunSuite with Matchers{


  test("Create Tuple (String -> Int)  from list of Cards and same list in string format ") {
    val boards: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades)))

    val hands: Hands = Hands(List(List(Card(Queen, Hearts), Card(Queen, Hearts)), List(Card(Six, Hearts), Card(Six, Hearts))))
    val listAllCardsString: List[String] = List("KhKsQsQh8s", "QhQh", "6h6h")

    val listOTuple: List[(String, List[Card])] = TexasHoldem.createListOfTupleHandListOfCards(boards, hands, listAllCardsString)
    val result = listOTuple.map(x => (x._1, TexasHoldem.getCardsValue(x._2)))
    result shouldBe List(("QhQh", 7001112), ("6h6h", 2012557))
  }

  test("Get Value of Card test") {
    val listOfCombination: List[Combination] = List(Flush, FourOfKind, FullHouse, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair)

    val listOfCards: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades), Card(Queen, Hearts), Card(Five, Hearts))

    val result: Int = TexasHoldem.getCardsValue(listOfCards)

    result shouldBe 6001112
  }

  test("Create answer from Tuple String -> Int TEST") {
    val board: Board = Board(List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades)))

    val hands: Hands = Hands(List(List(Card(Queen, Spades), Card(Queen, Hearts)), List(Card(Six, Hearts), Card(Six, Hearts)), List(Card(Queen, Hearts), Card(Queen, Hearts))))
    val listAllCardsString: List[String] = List("KhKsQsQh8s", "QsQh", "6h6h", "QhQh")

    val listOfTuple = TexasHoldem.createListOfTupleHandListOfCards(board, hands, listAllCardsString)

    val result: String = TexasHoldem.createAnswerFromTupleList(listOfTuple.map(x => (x._1, TexasHoldem.getCardsValue(x._2))))

    result shouldBe "6h6h QhQh=QsQh"
  }
}
