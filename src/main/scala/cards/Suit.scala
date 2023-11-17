package cards

import exeption.MyException
import exeption.MyException.IncorrectSuit

sealed trait Suit

object Suit{
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  def fromString(s: String): Either[MyException, Suit] = s match {
    case "h" => Right(Hearts)
    case "d" => Right(Diamonds)
    case "c" => Right(Clubs)
    case "s" => Right(Spades)
    case _ => Left(IncorrectSuit(s))
  }
}