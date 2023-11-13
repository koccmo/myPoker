package games


import cards.{Card, Rank, Suit}
import cards.Rank.{Four, Ten, Two}
import cards.Suit.Hearts
import combinations.{Combination, Flush, FourOfKind, FullHouse, HighCard, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair}
import exeption.MyException
import exeption.MyException.{WrongHandCardsNumberException, WrongTableCardsNumberException}

import scala.annotation.tailrec

object TexasHoldem {

  val listOfCombination: List[Combination] = List( StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, Pair, TwoPair)

  def checkBoardIncorrectLength(board: String): Boolean =
    board
      .split("")
      .toList
      .length != 10

  def checkHandsIncorrectLength(hands: List[String]): Boolean =
    hands
      .map(_.split("").toList)
      .exists(_.length != 4)

  def getListOfParsedRankAndSuitOrExceptionFromString(listOfBoardAndHandCards: List[String]): Either[MyException, List[Either[MyException, Object]]] = {
    listOfBoardAndHandCards match {
      case board :: _ if checkBoardIncorrectLength(board) => Left(WrongTableCardsNumberException())
      case _ :: hands if checkHandsIncorrectLength(hands) => Left(WrongHandCardsNumberException())
      case _ => Right(parseToRankAndSuitOrException(listOfBoardAndHandCards))
    }
  }

  def parseToRankAndSuitOrException(listStringCards: List[String]): List[Either[MyException, Object]] = {
    listStringCards.map(_.split("(?<=\\G..)")).flatMap(_.flatMap(_.zipWithIndex).map {
      case (char, index) if index % 2 == 0 => Rank.parse(char)
      case (char, index) if index % 2 == 1 => Suit.parse(char)
    })
  }

  def existListErrors(parsedString: Either[MyException,List[Either[MyException, Object]]]): Boolean =
    parsedString match {
      case Left(_) => true
      case Right(x) if x.exists(_.isLeft) => true
      case _ => false
    }

  def getException(parsedString: Either[MyException,List[Either[MyException, Object]]]): MyException =
    parsedString match {
      case Left(value) => value
      case Right(value) => value.collect{ case Left(x) => x }.head
    }

  def parseRankAndSuitToCards(listOfObject: List[Object]): List[Card] = {
    listOfObject.grouped(2).collect {
      case obRank :: obSuit :: Nil => (obRank, obSuit) match {
        case (rank: Rank, suit: Suit) => Some(Card(rank, suit))
        case _ => None
      }
    }.flatten.toList
  }

  def createListOfTupleHandListOfCards(listOfCards: List[Card],
                                       listOfBoardAndHandCards: List[String]): List[(String, List[Card])] = {
    val tableCards: List[Card] = listOfCards.take(5)

    val listOfHandCards: List[List[Card]] =
      listOfCards
        .drop(5)
        .grouped(2)
        .toList

    val listHandCardString: List[String] = listOfBoardAndHandCards.drop(1)

    def getListTuple(tableCards: List[Card],
                     listOfHandCards: List[List[Card]],
                     listHandCardString: List[String]): List[(String, List[Card])] = {
      @tailrec
      def helper(tableCards: List[Card],
                 listOfHandCards: List[List[Card]],
                 listHandCardString: List[String],
                 acc: List[(String, List[Card])]): List[(String, List[Card])] = {

        val newAcc: List[(String, List[Card])] = listOfHandCards match {
          case head :: _ => acc :+ (listHandCardString.head -> (tableCards ::: head))
          case _         => acc
        }

        listOfHandCards match {
          case _ :: tail => helper(tableCards, tail, listHandCardString.tail, newAcc)
          case _         => newAcc
        }
      }
      helper(tableCards, listOfHandCards, listHandCardString, List.empty)
    }
    getListTuple(tableCards, listOfHandCards, listHandCardString)
  }


  def getCardsValue(listOfCard: List[Card]): Int = {
    listOfCombination.collectFirst {
      case comb if comb.checkComb(listOfCard) => comb.getValueOfComb(listOfCard)
    }.getOrElse(HighCard.getValueOfComb(listOfCard))
  }


  def createAnswerFromTupleList(tupleList: List[(String, Int)]): String = {
    val sortedList = tupleList.sorted.sortBy(_._2)

    def createAnswer(sortedList: List[(String, Int)]): String = {
      @tailrec
      def helper(list: List[(String, Int)], accAnswer: String, accValue: Int): String = list match {
        case (comb, value) :: tail if accValue == 0     => helper(tail, accAnswer + s"$comb", value)
        case (comb, value) :: tail if accValue == value => helper(tail, accAnswer + s"=$comb", value)
        case (comb, value) :: tail if accValue != value => helper(tail, accAnswer + s" $comb", value)
        case _                                          => accAnswer
      }

      helper(sortedList, "", 0)
    }

    createAnswer(sortedList)
  }
//  @tailrec
//  def getValueOfCard(listOfCard: List[Card], listOfCombination: List[Combination]): Int = {
//    listOfCombination match {
//      case head :: _ if head.checkComb(listOfCard)     => head.getValueOfComb(listOfCard)
//      case head :: tail if !head.checkComb(listOfCard) => getValueOfCard(listOfCard, tail)
//      case _                                           => HighCard.getValueOfComb(listOfCard)
//    }
//  }

//  def getListTupleHandValue(listOfTupleHandListCard: List[(String, List[Card])]): List[(String, Int)] = {
//    listOfTupleHandListCard.map(x => (x._1, getValueOfCard(x._2, listOfCombination)))
//  }


  def getAnswer(listCharCards: List[String]): String = {
    val existMyExceptionInList: Boolean = existListErrors(getListOfParsedRankAndSuitOrExceptionFromString(listCharCards))

    existMyExceptionInList match {
      case true  => getException(getListOfParsedRankAndSuitOrExceptionFromString(listCharCards)).description
      case false =>
        val listOfParsedRankAndSuit = getListOfParsedRankAndSuitOrExceptionFromString(listCharCards)
          .map(x => x.collect { case Right(value) => value }) match {
          case Right(value) => value
        }

        val listOfCards: List[Card] = parseRankAndSuitToCards(listOfParsedRankAndSuit)

        val listOfTuplesStringListCard: List[(String, List[Card])] = createListOfTupleHandListOfCards(listOfCards, listCharCards)

        val listOfTupleStringInt: List[(String, Int)] = listOfTuplesStringListCard.map(x => (x._1, getCardsValue(x._2)))

        createAnswerFromTupleList(listOfTupleStringInt)
    }
  }


}
