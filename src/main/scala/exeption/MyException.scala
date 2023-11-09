package exeption

sealed trait MyException {
  def description: String
}
 object MyException {
   case class IncorrectRank(char: Char) extends MyException {
     override def description: String = s"Not correct Rank - ($char)"
   }
   case class IncorrectSuit(char: Char) extends MyException {
     override def description: String = s"Not correct Suit - ($char)"
   }
   case class WrongTableCardsNumberException() extends MyException {
     override def description: String = "Error: wrong table cards number"
   }
   case class WrongHandCardsNumberException() extends MyException {
     override def description: String = "Error: wrong hand cards number"
   }

 }