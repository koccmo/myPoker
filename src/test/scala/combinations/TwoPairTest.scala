package combinations

import domain.Card
import domain.Rank.{Ace, Queen, Seven, Six, Ten}
import domain.Suit.{Clubs, Diamonds, Hearts, Spades}
import gamesTypes.Combination.TwoPair
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
