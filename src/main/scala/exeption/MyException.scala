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
   // TODO: case class WrongHandStringLength() extends MyException =>
   //       case object WrongHandStringLength extends MyException
   case class WrongHandStringLength() extends MyException {
     override def description: String = "Error: wrong hand string length"
   }

   // TODO: case class WrongCardString() extends MyException =>
   //       case object WrongCardString extends MyException
   case class WrongCardString() extends MyException {
     override def description: String = s"Wrong card string"
   }

 }