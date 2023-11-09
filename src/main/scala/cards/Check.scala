package cards

import cards.Rank._
import cards.Suit._
import games.TexasHoldem

object Check extends App {

  val cardListStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
    Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Two, Hearts))

  val cardListNoStraightFlushFiveSimilarSuit: List[Card] = List(Card(Four, Hearts), Card(Nine, Spades), Card(Three, Hearts),
    Card(Ace, Hearts), Card(Five, Hearts), Card(Six, Spades), Card(Queen, Hearts))

  println(TexasHoldem.getListOfParsedRankAndSuitOrExceptionFromString(List("AFAFAFAFAF", "AAAA", "BBBB")))
  println(List("KhKsQsQh8s", "AAAA", "BBBB").map(_.grouped(2).toList))

  val listTableHandsString = List("KhKsQsQh8s", "Qh8s", "Th9s")
  val lazyList = LazyList.from(listTableHandsString).map { cards =>
    val lazyListCard = LazyList.from(cards.split("(?<=\\G..)")).flatMap { str =>
      str.zipWithIndex.collect {
        case (char, index) if index % 2 == 0 => Rank.parse(char)
        case (char, index) if index % 2 == 1 => Suit.parse(char)
      }
    }

    lazyListCard.find(_.isLeft) match {
      case Some(Left(err)) => Left(err)
      case _ => Right(lazyListCard.collect { case Right(value) => value })
    }

  }

  println(lazyList.collectFirst {
    case Right(value) => value
  } match {
    case Some(value) => value.foreach(println)
    case None => println("No Right values found")})

  lazyList.toList.foreach {
    case Left(err) => println(s"Error: $err")
    case Right(values) => println(s"Values: $values")
  }

  println("=========")

  lazyList.drop(2).collectFirst {
    case Right(value) => value
  } match {
    case Some(value) => value.foreach(println)
    case None => println("No Right values found")
  }

}
