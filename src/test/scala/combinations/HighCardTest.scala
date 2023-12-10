package combinations

import com.mypoker.domain.Card
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HighCardTest extends AnyFunSuite with Matchers {

  test("test"){
    val listOfSevenDifferentCards: List[Card] = List(Card(Ace, Hearts), Card(Seven, Hearts), Card(Two, Clubs),
      Card(Jack, Diamonds), Card(Nine, Spades), Card(Five, Spades), Card(Queen, Hearts))
//    val result: Int = HighCard.getCombStrength(listOfSevenDifferentCards)
//    result shouldEqual 526926
  }
}
