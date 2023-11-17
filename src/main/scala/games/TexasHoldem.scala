package games



import cards.{Board, Card, Hands, Rank, Suit}
import combinations.{Combination, Flush, FourOfKind, FullHouse, HighCard, Pair, Straight, StraightFlush, ThreeOfKind, TwoPair}
import exeption.MyException
import exeption.MyException.{WrongBoardStringLength, WrongCardString, WrongHandStringLength}

import scala.annotation.tailrec

object TexasHoldem {

  val listOfCombination: List[Combination] = List( StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, Pair, TwoPair)


  def validateBoard(board: String): Either[MyException, Board] = {
    def validateBoardSize(str: String): Either[MyException, List[String]] = {
      if (str.length == 10) Right(str.grouped(2).toList)
      else Left(WrongBoardStringLength(str.length))
    }

    def validateCard(str: String): Either[MyException, Card] = {
      str.split("").toList match {
        case r :: s :: Nil => for {
          rank <- Rank.fromString(r)
          suit <- Suit.fromString(s)
        } yield Card(rank, suit)
        case _ => Left(WrongCardString())
      }
    }

    validateBoardSize(board).flatMap { cardList => {
      val result = cardList.map(validateCard)
        .foldLeft((Option.empty[MyException], List.empty[Card])) { case ((exception, cards), value) =>
          value.fold(exception => (Some(exception), cards), card => (exception, cards :+ card))
        }

      val (exceptionOpt, cards) = result

      exceptionOpt match {
        case Some(value) => Left(value)
        case None => Right(Board(cards))
      }
     }
    }

  }


  def validateHands(hands: List[String]): Either[MyException, Hands] = {
    def validateSize(hands: List[String]): Either[MyException, List[String]] = {
      if (hands.forall(_.length == 4)) Right(hands.flatMap(_.grouped(2)))
      else Left(WrongHandStringLength())
    }

    def validateCard(hands: String): Either[MyException, Card] = {
      hands.split("").toList match {
        case r :: s :: Nil => for {
          rank <- Rank.fromString(r)
          suit <- Suit.fromString(s)
        } yield Card(rank, suit)
        case _ => Left(WrongCardString())
      }
    }

    validateSize(hands).flatMap( handsCard => {
      val result = handsCard.map(validateCard).foldLeft(
        (Option.empty[MyException], List.empty[Card])) { case ((error, cards), value) =>
      value.fold(exception => (Some(exception), cards), card => (error, cards :+ card))}

      val(exception, cards) = result

      exception match {
        case Some(value) => Left(value)
        case None => Right(Hands(cards.grouped(2).toList))
      }
    })

  }





//  def parseToRankAndSuitOrException(listStringCards: List[String]): List[Either[MyException, Object]] = {
//    listStringCards.map(_.split("(?<=\\G..)")).flatMap(_.flatMap(_.zipWithIndex).map {
//      case (char, index) if index % 2 == 0 => Rank.fromString(char.toString)
//      case (char, index) if index % 2 == 1 => Suit.fromString(char.toString)
//    })
//  }

//  def existListErrors(parsedString: Either[MyException, List[Either[MyException, Object]]]): Boolean =
//    parsedString match {
//      case Left(_) => true
//      case Right(x) if x.exists(_.isLeft) => true
//      case _ => false
//    }

//  def getException(parsedString: Either[MyException, List[Either[MyException, Object]]]): MyException =
//    parsedString match {
//      case Left(value) => value
//      case Right(value) => value.collect { case Left(x) => x }.head
//    }

//  def parseRankAndSuitToCards(listOfObject: List[Object]): List[Card] = {
//    listOfObject.grouped(2).collect {
//      case obRank :: obSuit :: Nil => (obRank, obSuit) match {
//        case (rank: Rank, suit: Suit) => Some(Card(rank, suit))
//        case _ => None
//      }
//    }.flatten.toList
//  }

  def createListOfTupleHandListOfCards(board: Board, hands: Hands,
                                       handsString: List[String]): List[(String, List[Card])] = {


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

    getListTuple(board.getCards, hands.getCards, handsString)
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


  def getAnswer(board: String, hands: List[String]): String = {
    validateBoard(board) match {
      case Left(exception)   => exception.description
      case Right(boardCards) => validateHands(hands) match {
        case Left(exception)  => exception.description
        case Right(handCards) =>  createAnswerFromTupleList(
          createListOfTupleHandListOfCards(boardCards, handCards, hands).map(x => (x._1, getCardsValue(x._2))))
      }
    }

  }


}
