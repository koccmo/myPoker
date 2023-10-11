package combinations

import cards.Card
import cards.Rank._
import cards.Suit._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FullHouseTest extends AnyFunSuite with Matchers {

  test("One of three Of Kind and One pair"){
    val listCardOneThreeOfKindOnePair: List[Card] = List(Card(Ten, Diamonds), Card(Nine, Spades), Card(Jack, Hearts),
      Card(Six, Spades), Card(Nine, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = FullHouse.checkComb(listCardOneThreeOfKindOnePair)

    result shouldEqual true
  }

  test("One of three Of Kind and Two pair") {
    val listCardOneThreeOfKindOnePair: List[Card] = List(Card(Ten, Diamonds), Card(Nine, Spades), Card(Ten, Hearts),
      Card(Six, Spades), Card(Nine, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = FullHouse.checkComb(listCardOneThreeOfKindOnePair)

    result shouldEqual true
  }

  test("Two of three Of Kind") {
    val listCardOneThreeOfKindOnePair: List[Card] = List(Card(Six, Diamonds), Card(Nine, Spades), Card(Jack, Hearts),
      Card(Six, Spades), Card(Nine, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = FullHouse.checkComb(listCardOneThreeOfKindOnePair)

    result shouldEqual true
  }

  test("No three of Kind, Two Pair"){
    val listCardNoThreeOfKindTwoPair: List[Card] = List(Card(Ace, Diamonds), Card(Two, Spades), Card(Jack, Hearts),
      Card(Six, Spades), Card(Nine, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = FullHouse.checkComb(listCardNoThreeOfKindTwoPair)

    result shouldEqual false
  }

  test("Two times with Three similar cards") {
    val listWithTwoTimesThreeOfKind: List[Card] = List(Card(Ace, Diamonds), Card(Two, Spades), Card(Jack, Hearts),
      Card(Ace, Spades), Card(Two, Hearts), Card(Ace, Hearts), Card(Two, Hearts))

    val result: Int = FullHouse.getValueOfComb(listWithTwoTimesThreeOfKind)

    result shouldEqual 6001301
  }

  test("One Three of Kind and two pair"){
    val listCardOneThreeOfKindTwoPair: List[Card] = List(Card(Ace, Diamonds), Card(Two, Spades), Card(Jack, Hearts),
      Card(Ace, Spades), Card(Two, Hearts), Card(Ace, Hearts), Card(Jack, Hearts))

    val result: Int = FullHouse.getValueOfComb(listCardOneThreeOfKindTwoPair)

    result shouldEqual 6001310
  }

  test("One Three of Kind and One Pair"){
    val listOneThreeOfKindOnePair: List[Card] = List(Card(Nine, Diamonds), Card(Ten, Spades), Card(Jack, Hearts),
      Card(Nine, Spades), Card(Ten, Hearts), Card(Nine, Hearts), Card(Two, Hearts))

    val result: Int = FullHouse.getValueOfComb(listOneThreeOfKindOnePair)

    result shouldEqual 6000809
  }
}
