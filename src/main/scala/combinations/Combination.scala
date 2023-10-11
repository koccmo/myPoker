package combinations

import cards.Card

trait Combination {
  def startCombValue: Int

  def checkComb(listOfCards: List[Card]): Boolean

  def getValueOfComb(listOfCards: List[Card]): Int
}
