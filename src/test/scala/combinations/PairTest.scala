package combinations

import domain.Card
import domain.Rank.{Ace, Jack, Nine, Queen, Seven, Six}
import domain.Suit.{Clubs, Diamonds, Hearts, Spades}
import gamesTypes.Combination.Pair
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PairTest extends AnyFunSuite with Matchers{

  test("Pair checkComb with one pair list cards"){
    val listOfCardsWithOnePair: List[Card] = List(Card(Six, Hearts), Card(Seven, Hearts), Card(Ace, Clubs),
      Card(Jack, Diamonds), Card(Nine, Spades), Card(Ace, Spades), Card(Queen, Hearts))
    val result: Boolean = Pair.checkComb(listOfCardsWithOnePair)
    result shouldEqual true
  }

  test("Pair checkComb with two pair list cards"){
    val listOfCardsWithTwoPair: List[Card] = List(Card(Six, Hearts), Card(Six, Hearts), Card(Ace, Clubs),
      Card(Jack, Diamonds), Card(Nine, Spades), Card(Ace, Spades), Card(Queen, Hearts))
    val result: Boolean = Pair.checkComb(listOfCardsWithTwoPair)
    result shouldEqual false
  }
}
