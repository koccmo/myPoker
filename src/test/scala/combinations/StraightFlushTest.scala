package combinations

import cards.Card
import cards.Rank._
import cards.Suit._
import combinations.Combination.StraightFlush
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StraightFlushTest extends AnyFunSuite with Matchers {

  test("True test with Straight Flush with 5 similar Suit"){
    val cardListStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Two, Hearts))

    val result: Boolean = StraightFlush.checkComb(cardListStraightFlushFiveSimilarSuit)

    result shouldEqual true
  }

  test("False test five similar suite not Straight"){
    val cardListNoStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Queen, Hearts))

    val result: Boolean = StraightFlush.checkComb(cardListNoStraightFlushFiveSimilarSuit)

    result shouldEqual false
  }

  test("True test six similar suite and StraightFlush"){
    val cardListStraightFlushSixSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Hearts), Card(Two, Hearts))

    val result: Boolean = StraightFlush.checkComb(cardListStraightFlushSixSimilarSuit)

    result shouldEqual true
  }

  test("Test with 6 card similar suite and two StraightFlush comb"){
    val cardListStraightFlushSixSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Hearts), Card(Two, Hearts))

    val result: Int = StraightFlush.getValueOfComb(cardListStraightFlushSixSimilarSuit)

    result shouldEqual 8000002
  }

  test("Test StraightFlush start from Ace"){
    val cardListStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Two, Hearts))

    val result: Int = StraightFlush.getValueOfComb(cardListStraightFlushFiveSimilarSuit)

    result shouldEqual 8000001
  }

  test("Test 6 similar suit 1 StraightFlush"){
    val cardListSixSimilarSuitOneStraightFlush: List[Card] = List(Card(Seven, Hearts), Card(Nine, Spades), Card(Eight, Hearts),
      Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Hearts), Card(Nine, Hearts))

    val result: Int = StraightFlush.getValueOfComb(cardListSixSimilarSuitOneStraightFlush)

    result shouldEqual 8000005
  }

}
