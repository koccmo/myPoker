package combinations

import cards.{Card, CardRankValue, Rank}
import cards.Rank._
import cards.Suit._

object Check extends App {

  val cardListStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
    Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Two, Hearts))

  val cardListNoStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
    Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Queen, Hearts))

  def make(cardsList: List[Card]) = {
    cardsList
      .groupBy(_.suit)
      .exists(_._2.length >= 5)
  }

  val ok: List[Rank] = {
    cardListStraightFlushFiveSimilarSuit
      .groupBy(_.suit)
      .filter(_._2.length >= 5)
      .values
      .flatMap(_.map(_.rank))
      .toList
  }


  println(make(cardListStraightFlushFiveSimilarSuit))
  println(make(cardListNoStraightFlushFiveSimilarSuit))

  println(StraightFlush.straight.keys.toList.map(_.intersect(ok)).exists(_.length == 5))
}
