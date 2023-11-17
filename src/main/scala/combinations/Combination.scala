package combinations

import cards.Card

// TODO it is supposed to be sealed trait
trait Combination {
  def startCombValue: Int
  def checkComb(listOfCards: List[Card]): Boolean
  def getValueOfComb(listOfCards: List[Card]): Int
}
