package cards
import validation.ValidationError
import validation.ValidationError.IncorrectRank

sealed trait Rank

object Rank{
  case object Two extends Rank {
    override def toString: String = "2"
  }
  case object Three extends Rank {
    override def toString: String = "3"
  }
  case object Four extends Rank {
    override def toString: String = "4"
  }
  case object Five extends Rank {
    override def toString: String = "5"
  }
  case object Six extends Rank {
    override def toString: String = "6"
  }
  case object Seven extends Rank {
    override def toString: String = "7"
  }
  case object Eight extends Rank {
    override def toString: String = "8"
  }
  case object Nine extends Rank {
    override def toString: String = "9"
  }
  case object Ten extends Rank {
    override def toString: String = "T"
  }
  case object Jack extends Rank {
    override def toString: String = "J"
  }
  case object Queen extends Rank {
    override def toString: String = "Q"
  }
  case object King extends Rank {
    override def toString: String = "K"
  }
  case object Ace extends Rank {
    override def toString: String = "A"
  }

 def fromString(s: String): Either[ValidationError, Rank] = s match {
   case "2" => Right(Two)
   case "3" => Right(Three)
   case "4" => Right(Four)
   case "5" => Right(Five)
   case "6" => Right(Six)
   case "7" => Right(Seven)
   case "8" => Right(Eight)
   case "9" => Right(Nine)
   case "T" => Right(Ten)
   case "J" => Right(Jack)
   case "Q" => Right(Queen)
   case "K" => Right(King)
   case "A" => Right(Ace)
   case _ => Left(IncorrectRank(s))
 }

}

