package combinations

import cards.Card
import cards.Rank.{Ace, Queen, Seven, Six, Ten}
import cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TwoPairTest extends AnyFunSuite with Matchers {

  test("Two pair test"){
    val listOfCards: List[Card] = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Ace, Clubs),
      Card(Six, Diamonds), Card(Seven, Spades), Card(Ace, Spades), Card(Queen, Hearts))

    val result: Int = TwoPair.getValueOfComb(listOfCards)

    result shouldEqual 2013311
  }

  test("Three pair test"){
    val listOfCards: List[Card] = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Ace, Clubs),
      Card(Six, Diamonds), Card(Seven, Spades), Card(Ace, Spades), Card(Ten, Hearts))
    val result: Int = TwoPair.getValueOfComb(listOfCards)

    result shouldEqual 2013456
  }
}
