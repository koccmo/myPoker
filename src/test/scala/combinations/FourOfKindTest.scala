package combinations

import domain.Card
import domain.Rank._
import domain.Suit._
import gamesTypes.Combination.FourOfKind
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FourOfKindTest extends AnyFunSuite with Matchers {

  test("True test for Four Of Kind in Cards"){
    val cardListWithFourOfKind: List[Card] = List(Card(Ten, Hearts), Card(Nine, Hearts), Card(Jack, Hearts),
      Card(Nine, Hearts), Card(Nine, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = FourOfKind.checkComb(cardListWithFourOfKind)

    result shouldEqual true
  }

  test("False test no four of Kind in card"){
    val cardListNoFourOfKind: List[Card] = List(Card(Ten, Hearts), Card(Nine, Hearts), Card(Jack, Hearts),
      Card(Nine, Hearts), Card(Nine, Hearts), Card(Six, Hearts), Card(Two, Hearts))

    val result: Boolean = FourOfKind.checkComb(cardListNoFourOfKind)

    result shouldEqual false
  }

  test("Four Ace and Queen test"){
    val cardListFourOfKindAce: List[Card] = List(Card(Queen, Hearts), Card(Ace, Hearts), Card(Ace, Hearts),
      Card(Nine, Hearts), Card(Ace, Hearts), Card(Six, Hearts), Card(Ace, Hearts))

    val result: Int = FourOfKind.getValueOfComb(cardListFourOfKindAce)

    result shouldEqual 7001311
  }

  test("Four Two and Ten"){
    val cardListFourOfKindTwoAndTen: List[Card] = List(Card(Eight, Hearts), Card(Two, Hearts), Card(Two, Hearts),
      Card(Nine, Hearts), Card(Two, Hearts), Card(Ten, Hearts), Card(Two, Hearts))

    val result: Int = FourOfKind.getValueOfComb(cardListFourOfKindTwoAndTen)

    result shouldEqual 7000109
  }

  test("Four Ten and Pair six and Five"){
    val cardListFourOfKindTenAndSix: List[Card] = List(Card(Ten, Hearts), Card(Five, Hearts), Card(Ten, Hearts),
      Card(Ten, Hearts), Card(Six, Hearts), Card(Ten, Hearts), Card(Six, Hearts))

    val result: Int = FourOfKind.getValueOfComb(cardListFourOfKindTenAndSix)

    result shouldEqual 7000905
  }
}
