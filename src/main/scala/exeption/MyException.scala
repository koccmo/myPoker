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
   case class WrongTableCardsNumberException() extends MyException {
     override def description: String = "Error: wrong table cards number"
   }
   case class WrongHandCardsNumberException() extends MyException {
     override def description: String = "Error: wrong hand cards number"
   }

 }