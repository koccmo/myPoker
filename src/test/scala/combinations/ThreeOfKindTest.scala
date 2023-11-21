package combinations

import cards.Card
import cards.Rank._
import cards.Suit._
import combinations.Combination.ThreeOfKind
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ThreeOfKindTest extends AnyFunSuite with Matchers {

  test("Combination check True") {
    val listOfCardWithTwoTimesThreeOfKind: List[Card] = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Ace, Clubs),
      Card(Ten, Diamonds), Card(Seven, Spades), Card(Seven, Spades), Card(Ten, Hearts))
    val result: Boolean = ThreeOfKind.checkComb(listOfCardWithTwoTimesThreeOfKind)

    result shouldEqual true
  }

  test("No Three of Kind Check Comb Test") {
    val listOfCardWithTwoTimesThreeOfKind: List[Card] = List(Card(Nine, Hearts), Card(Queen, Hearts), Card(Ace, Clubs),
      Card(Ten, Diamonds), Card(Jack, Spades), Card(Seven, Spades), Card(Ten, Hearts))
    val result: Boolean = ThreeOfKind.checkComb(listOfCardWithTwoTimesThreeOfKind)

    result shouldEqual false
  }

  test("Two times of Three of Kind Test"){
    val listOfCardWithTwoTimesThreeOfKind: List[Card] = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Ace, Clubs),
      Card(Ten, Diamonds), Card(Seven, Spades), Card(Seven, Spades), Card(Ten, Hearts))
    val result: Int = ThreeOfKind.getValueOfComb(listOfCardWithTwoTimesThreeOfKind)

    result shouldEqual 3009201
  }

  test("One times of Three of Kind Test"){
    val listOfCardWithTwoTimesThreeOfKind: List[Card] = List(Card(Ten, Hearts), Card(Queen, Hearts), Card(Ace, Clubs),
      Card(Ten, Diamonds), Card(Jack, Spades), Card(Seven, Spades), Card(Ten, Hearts))
    val result: Int = ThreeOfKind.getValueOfComb(listOfCardWithTwoTimesThreeOfKind)

    result shouldEqual 3009206
  }

}
