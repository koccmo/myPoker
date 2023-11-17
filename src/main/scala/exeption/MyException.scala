package exeption

sealed trait MyException {
  def description: String
}
 object MyException {
   case class IncorrectRank(s: String) extends MyException {
     override def description: String = s"Not correct Rank - ($s)"
   }
   case class IncorrectSuit(s: String) extends MyException {
     override def description: String = s"Not correct Suit - ($s)"
   }
   case class WrongBoardStringLength(length: Int) extends MyException {
     override def description: String = s"Error: wrong board string length - ($length)"
   }
   case class WrongHandStringLength() extends MyException {
     override def description: String = "Error: wrong hand string length"
   }

   case class WrongCardString() extends MyException {
     override def description: String = s"Wrong card string"
   }

 }