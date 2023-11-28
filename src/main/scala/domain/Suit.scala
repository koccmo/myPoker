package domain
import validation.ValidationError
import validation.ValidationError.IncorrectSuit

sealed trait Suit

object Suit{
  case object Clubs extends Suit {
    override def toString: String = "c"
  }
  case object Diamonds extends Suit {
    override def toString: String = "d"
  }
  case object Hearts extends Suit {
    override def toString: String = "h"
  }
  case object Spades extends Suit {
    override def toString: String = "s"
  }

  def fromString(s: String): Either[ValidationError, Suit] = s match {
    case "h" => Right(Hearts)
    case "d" => Right(Diamonds)
    case "c" => Right(Clubs)
    case "s" => Right(Spades)
    case _ => Left(IncorrectSuit(s))
  }
}