package games

import cards.Card
import cards.Rank.{Eight, Five, King, Nine, Queen, Six, Ten}
import cards.Suit.{Hearts, Spades}
import combinations.{Combination, Flush, FourOfKind, FullHouse, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair}
import exeption.MyException.{WrongHandCardsNumberException, WrongTableCardsNumberException}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TexasHoldemTest extends AnyFunSuite with Matchers{

  test(" True check long of board if is 10 chars"){
    val boardCard: String = "KhKsQsQh8s"
    val result: Boolean = TexasHoldem.checkBoardIncorrectLength(boardCard)
    result shouldEqual false
  }

  test("False check board long if is 10 chars"){
    val boardCard: String = "KhKsQsQh8"
    val result: Boolean = TexasHoldem.checkBoardIncorrectLength(boardCard)
    result shouldEqual true
  }

  test("False check if any hands long is no 4 char"){
    val listOfHands: List[String] = List("KcQd", "KdQc", "4sTh", "Ts5h", "Jh6s")
    val result: Boolean = TexasHoldem.checkHandsIncorrectLength(listOfHands)
    result shouldEqual false
  }

  test("True check if any hands long is no 4 char"){
    val listOfHands: List[String] = List("KcQd", "KdQc", "4sT", "Ts5h", "Jh6s")
    val result: Boolean = TexasHoldem.checkHandsIncorrectLength(listOfHands)
    result shouldEqual true
  }

  test("Not correct long table cards test, return Exception"){
    val result =
      TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(List("AF", "AAAA"))

    result shouldBe Left(WrongTableCardsNumberException())
  }

  test("Not correct hand long cards test, return Exception") {
    val result =
      TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(List("AFAFAFAFAF", "AAAA", "BBB"))

    result shouldBe Left(WrongHandCardsNumberException())
  }

  test("Not correct long Table And Hand Card, return Exception - Not correct table cards long"){
    val result =
      TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(List("AFAFAFAFA", "AAAA", "BBB"))

    result shouldBe Left(WrongTableCardsNumberException())
  }

  test("Check exist list Errors - True"){
    val stringListCards: List[String] = List("phKsQsQh8s", "Qh8s", "Th9s")
    val result = TexasHoldem.existListErrors(TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(stringListCards))

    result shouldBe true
  }

  test("Check exist list Errors - False") {
    val stringListCards: List[String] = List("KhKsQsQh8s", "Qh8s", "Th9s")
    val result = TexasHoldem.existListErrors(TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(stringListCards))

    result shouldBe false
  }

  test("Parse List of Rank and Suit to Cards"){
    val listOfRankAndSuit = List(King, Hearts, King, Spades, Queen,
      Spades, Queen, Hearts, Eight, Spades, Queen, Hearts, Eight, Spades, Ten, Hearts, Nine, Spades)

    val result = TexasHoldem.parseRankAndSuitToCards(listOfRankAndSuit)

    result shouldBe List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades),
      Card(Queen, Hearts), Card(Eight, Spades), Card(Queen, Hearts), Card(Eight, Spades), Card(Ten, Hearts), Card(Nine, Spades))
  }

//  test ("Create Tuple (String -> ListCard)") {
//    val listOfTableAndHands: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
//      Card(Eight, Spades), Card(Queen, Hearts), Card(Queen, Hearts), Card(Six, Hearts), Card(Six, Hearts))
//    val listAllCardsString: List[String] = List("KhKsQsQh8s", "QhQh", "6h6h")
//
//    val result = TexasHoldem.createListOfTupleHandListOfCards(listOfTableAndHands, listAllCardsString)
//
//    result shouldBe List(("QhQh", List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
//      Card(Eight, Spades), Card(Queen, Hearts), Card(Queen, Hearts))), ("6h6h", List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
//      Card(Eight, Spades), Card(Six, Hearts), Card(Six, Hearts))))
//  }

  test("Create Tuple (String -> Int)  from list of Cards and same list in string format ") {
    val listOfTableAndHands: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades), Card(Queen, Hearts), Card(Queen, Hearts), Card(Six, Hearts), Card(Six, Hearts))
    val listAllCardsString: List[String] = List("KhKsQsQh8s", "QhQh", "6h6h")

    val listOTuple: List[(String, List[Card])] = TexasHoldem.createListOfTupleHandListOfCards(listOfTableAndHands, listAllCardsString)

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
    val listOfTableAndHands: List[Card] = List(Card(King, Hearts), Card(King, Spades), Card(Queen, Spades), Card(Queen, Hearts),
      Card(Eight, Spades), Card(Queen, Spades), Card(Queen, Hearts), Card(Six, Hearts), Card(Six, Hearts), Card(Queen, Hearts), Card(Queen, Hearts))

    val listAllCardsString: List[String] = List("KhKsQsQh8s", "QsQh", "6h6h", "QhQh")

    val listOfTuple = TexasHoldem.createListOfTupleHandListOfCards(listOfTableAndHands, listAllCardsString)

    val result: String = TexasHoldem.createAnswerFromTupleList(listOfTuple.map(x => (x._1, TexasHoldem.getCardsValue(x._2))))

    result shouldBe "6h6h QhQh=QsQh"
  }
}
