package cards

import exeption.MyException
import exeption.MyException.IncorrectRank

sealed trait Rank

object Rank{
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

 def parse(ch: Char): Either[MyException, Rank] = ch match {
   case '2' => Right(Two)
   case '3' => Right(Three)
   case '4' => Right(Four)
   case '5' => Right(Five)
   case '6' => Right(Six)
   case '7' => Right(Seven)
   case '8' => Right(Eight)
   case '9' => Right(Nine)
   case 'T' => Right(Ten)
   case 'J' => Right(Jack)
   case 'Q' => Right(Queen)
   case 'K' => Right(King)
   case 'A' => Right(Ace)
   case _ => Left(IncorrectRank(ch))
 }


}

