package tests

import cards.{CardRankValue, Rank}
import cards.Rank.{Ace, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CardRankValue extends AnyFunSuite with Matchers{
  test("one rank value test"){
    val rankList: List[Rank] = List(Ace)
    val result: Int = CardRankValue.getValueOfCards(rankList)
    result shouldEqual 13
  }

  test("two ranks value test"){
    val listOfTwoRanks: List[Rank] = List(Ace, Queen)
    val result: Int = CardRankValue.getValueOfCards(listOfTwoRanks)
    result shouldEqual 206
  }

  test("three ranks value test"){
    val listOfThreeRanks: List[Rank] = List(King, Ten, Five)
    val result: Int = CardRankValue.getValueOfCards(listOfThreeRanks)
    result shouldEqual 2539
  }

  test("four ranks value test"){
    val listOfFourRanks: List[Rank] = List(Jack, Nine, Four, Two)
    val result: Int = CardRankValue.getValueOfCards(listOfFourRanks)
    result shouldEqual 29646
  }

  test("five ranks value test"){
    val listOfFiveRanks: List[Rank] = List(Ace, Eight, Seven, Six, Three)
    val result: Int = CardRankValue.getValueOfCards(listOfFiveRanks)
    result shouldEqual 514877
  }
}
