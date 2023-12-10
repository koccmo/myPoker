package combinations

import com.mypoker.domain.Card
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StraightTest extends AnyFunSuite with Matchers {


  test("Straight combination check True"){

    val listOfCardWithStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

//    val result : Boolean = Straight.checkComb(listOfCardWithStraightComb)
//
//    result shouldEqual true
  }


  test("Straight combination check False"){

    val listOfCardWithStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Ace, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

//    val result: Boolean = Straight.checkComb(listOfCardWithStraightComb)
//
//    result shouldEqual false
  }


  test("One Straight Comb test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

//    val result: Int = Straight.getCombStrength(listOfCardWithOneStraightComb)
//
//    result shouldEqual 4000002
  }


  test("Two Straight Comb Test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Seven, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Nine, Hearts))

//    val result: Int = Straight.getCombStrength(listOfCardWithOneStraightComb)
//
//    result shouldEqual 4000003
  }


  test("Three Straight Comb Test"){

    val listOfCardWithOneStraightComb: List[Card] = List(Card(Seven, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Six, Spades), Card(Ace, Hearts))

//    val result: Int = Straight.getCombStrength(listOfCardWithOneStraightComb)
//
//    result shouldEqual 4000003
  }


  test("Straight Comb Start From Ace Test"){

    val listOfCardWithStraightStartFromAce: List[Card] = List(Card(Nine, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Spades), Card(Jack, Spades), Card(Ace, Hearts))

//    val result: Int = Straight.getCombStrength(listOfCardWithStraightStartFromAce)
//
//    result shouldEqual 4000001
  }

  test("Straight Comb Start From 4 Test") {

    val listOfCardStraightStartFromFour: List[Card] = List(Card(Four, Clubs), Card(King, Spades), Card(Four, Hearts),
      Card(Eight, Spades), Card(Seven, Spades), Card(Five, Diamonds), Card(Six, Diamonds))

//    val result: Int = Straight.getCombStrength(listOfCardStraightStartFromFour)
//
//    result shouldEqual 4000004
  }

}
