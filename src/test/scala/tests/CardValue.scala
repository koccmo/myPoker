package tests

import com.mypoker.CardValue
import com.mypoker.domain.Rank
import com.mypoker.domain.Rank._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CardValue extends AnyFunSuite with Matchers{
  test("one rank value test"){
    val rankList: List[Rank] = List(Ace)
    val result: Int = CardValue.getValue(rankList)
    result shouldEqual 13
  }

  test("two ranks value test"){
    val listOfTwoRanks: List[Rank] = List(Ace, Queen)
    val result: Int = CardValue.getValue(listOfTwoRanks)
    result shouldEqual 206
  }

  test("three ranks value test"){
    val listOfThreeRanks: List[Rank] = List(King, Ten, Five)
    val result: Int = CardValue.getValue(listOfThreeRanks)
    result shouldEqual 2539
  }

  test("four ranks value test"){
    val listOfFourRanks: List[Rank] = List(Jack, Nine, Four, Two)
    val result: Int = CardValue.getValue(listOfFourRanks)
    result shouldEqual 29646
  }

  test("five ranks value test"){
    val listOfFiveRanks: List[Rank] = List(Ace, Eight, Seven, Six, Three)
    val result: Int = CardValue.getValue(listOfFiveRanks)
    result shouldEqual 514877
  }
}
