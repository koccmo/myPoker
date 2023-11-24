package combinations

import cards.Card
import cards.Rank.{Ace, Five, Jack, Nine, Queen, Seven, Two}
import cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import combinations.Combination.HighCard
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HighCardTest extends AnyFunSuite with Matchers {

  test("test"){
    val listOfSevenDifferentCards: List[Card] = List(Card(Ace, Hearts), Card(Seven, Hearts), Card(Two, Clubs),
      Card(Jack, Diamonds), Card(Nine, Spades), Card(Five, Spades), Card(Queen, Hearts))
    val result: Int = HighCard.getValueOfComb(listOfSevenDifferentCards)
    result shouldEqual 526926
  }
}
