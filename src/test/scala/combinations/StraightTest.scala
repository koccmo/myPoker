package combinations

import cards.Card
import cards.Rank._
import cards.Suit._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StraightTest extends AnyFunSuite with Matchers {


  test("Straight combination check True"){

    val listOfCardWithStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

    val result : Boolean = Straight.checkComb(listOfCardWithStraightComb)

    result shouldEqual true
  }


  test("Straight combination check False"){

    val listOfCardWithStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Ace, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

    val result: Boolean = Straight.checkComb(listOfCardWithStraightComb)

    result shouldEqual false
  }


  test("One Straight Comb test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

    val result: Int = Straight.getValueOfComb(listOfCardWithOneStraightComb)

    result shouldEqual 4000002
  }


  test("Two Straight Comb Test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Seven, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

    val result: Int = Straight.getValueOfComb(listOfCardWithOneStraightComb)

    result shouldEqual 4000003
  }


  test("Three Straight Comb Test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Seven, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Ace, Hearts))

    val result: Int = Straight.getValueOfComb(listOfCardWithOneStraightComb)

    result shouldEqual 4000003
  }


  test("Straight Comb Start From Ace Test"){

    val listOfCardWithStraightStartFromAce: List[Card] = List(Card(Nine, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Jack, Spades), Card(Ace, Hearts))

    val result: Int = Straight.getValueOfComb(listOfCardWithStraightStartFromAce)

    result shouldEqual 4000001
  }

}
