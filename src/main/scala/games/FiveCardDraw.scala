package games

import combinations.{Combination, Flush, FourOfKind, FullHouse, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair}
import exeption.MyException
import exeption.MyException.WrongHandCardsNumberException

object FiveCardDraw {

  val listOfCombination: List[Combination] = List( StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, Pair, TwoPair)

  def checkListOfHandsIsCorrectLength(listOfHand: List[String]): Boolean =
    listOfHand
      .map(_.split(""))
      .exists(_.length != 10)

//  def getListOfRankSuitOfException(listOfHand: List[String]): Either[MyException, List[Object]] = {
//    checkListOfHandsIsCorrectLength(listOfHand) match {
//      case true  => Left(WrongHandCardsNumberException())
//      case false =>
//    }
//  }
}
