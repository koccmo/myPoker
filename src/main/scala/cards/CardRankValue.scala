package cards

import cards.Rank.{Ace, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}

import scala.annotation.tailrec

object CardRankValue {
  val firstLapCard: Map[Rank, Int] = Map(Two -> 38000, Three -> 76000, Four -> 114000, Five -> 152000, Six -> 190000,
    Seven -> 228000, Eight -> 266000, Nine -> 304000, Ten -> 342000, Jack -> 380000, Queen -> 418000, King -> 456000, Ace -> 494000)

  val secondLapCard: Map[Rank, Int] = Map(Two -> 2800, Three -> 5600, Four -> 8400, Five -> 11200, Six -> 14000,
    Seven -> 16800, Eight -> 19600, Nine -> 22400, Ten -> 25200, Jack -> 28000, Queen -> 30800, King -> 33600, Ace -> 36400)

  val thirdLapCard: Map[Rank, Int] = Map(Two -> 200, Three -> 400, Four -> 600, Five -> 800, Six -> 1000,
    Seven -> 1200, Eight -> 1400, Nine -> 1600, Ten -> 1800, Jack -> 2000, Queen -> 2200, King -> 2400, Ace -> 2600)

  val fourthLapCard: Map[Rank, Int] = Map(Two -> 15, Three -> 30, Four -> 45, Five -> 60, Six -> 75,
    Seven -> 90, Eight -> 105, Nine -> 120, Ten -> 135, Jack -> 150, Queen -> 165, King -> 180, Ace -> 195)

  val cardRankValue: Map[Rank, Int] = Map(Two -> 1, Three -> 2, Four -> 3, Five -> 4, Six -> 5, Seven -> 6, Eight -> 7, Nine -> 8,
    Ten -> 9, Jack -> 10, Queen -> 11, King -> 12, Ace -> 13)

  def getValueOfCards(listOfCards: List[Rank]): Int = {
    @tailrec
    def helper(listOfCards: List[Rank], acc: Int): Int = {
      val newAcc = listOfCards.length match {
        case 5 => firstLapCard(listOfCards.head) + acc
        case 4 => secondLapCard(listOfCards.head) + acc
        case 3 => thirdLapCard(listOfCards.head) + acc
        case 2 => fourthLapCard(listOfCards.head) + acc
        case 1 => cardRankValue(listOfCards.head) + acc
        case _ => acc
      }

      listOfCards match {
        case _ :: tail => helper(tail, newAcc)
        case _ => newAcc
      }
    }

    helper(listOfCards, 0)
  }

}
