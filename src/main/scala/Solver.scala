import gamesType.TexasHoldem
import validation.Validator

object Solver {
  // TODO: implement solution logic
  def process(line: String): String = {

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: listHand => val result =  for {
        board <- Validator.validateBoard(board)
        hands <- Validator.validateHands(listHand)
      } yield TexasHoldem.getAnswer(board, hands, listHand)

        result match {
          case Left(error)  => error.description
          case Right(value) => value
        }

      case "omaha-holdem" :: board :: hands => "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => "The solution doesn't support Five Card Draw"
      case x :: _ => "Unrecognized game type"
      case _ => "Invalid input"
    }
  }

}
