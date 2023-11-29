package combinations

import com.mypoker.Combination.Flush
import com.mypoker.domain.Card
import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FlushTest extends AnyFunSuite with Matchers {


  test("Check true comb of Flush with five similar Suits"){
    val listCardWithFiveSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Hearts), Card(Four, Hearts), Card(Six, Spades), Card(Nine, Hearts))

    val result: Boolean = Flush.checkComb(listCardWithFiveSimilarSuits)

    result shouldEqual true
  }

  test("Check false comb Of Flush with 4 similar Suits"){
    val listCardsWithFourSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Clubs),
      Card(Three, Diamonds), Card(Four, Hearts), Card(Six, Spades), Card(Nine, Hearts))

    val result: Boolean = Flush.checkComb(listCardsWithFourSimilarSuits)

    result shouldEqual false
  }

  test("Check true comb of Flush with six similar Suits"){
    val listCardsWithSixSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Two, Hearts), Card(Five, Hearts),
      Card(Three, Diamonds), Card(Four, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Boolean = Flush.checkComb(listCardsWithSixSimilarSuits)

    result shouldEqual true
  }

  test("Five card with similar Suits"){
    val listCardFiveSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Nine, Hearts), Card(Jack, Hearts),
      Card(Three, Diamonds), Card(Four, Hearts), Card(Six, Hearts), Card(Nine, Spades))

    val result: Int = Flush.getValueOfComb(listCardFiveSimilarSuits)

    result shouldEqual 5406878
  }

  test("Six card with similar Suits"){
    val listCardSixSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Nine, Hearts), Card(Jack, Hearts),
      Card(Three, Hearts), Card(Four, Hearts), Card(Six, Hearts), Card(Nine, Spades))

    val result: Int = Flush.getValueOfComb(listCardSixSimilarSuits)

    result shouldEqual 5406878
  }

  test("Seven cards with similar Suits"){
    val listCardSevenSimilarSuits: List[Card] = List(Card(Ten, Hearts), Card(Nine, Hearts), Card(Jack, Hearts),
      Card(Three, Hearts), Card(Four, Hearts), Card(Six, Hearts), Card(Two, Hearts))

    val result: Int = Flush.getValueOfComb(listCardSevenSimilarSuits)

    result shouldEqual 5406878
  }


}
