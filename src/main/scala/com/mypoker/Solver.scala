package com.mypoker

import com.mypoker.gametypes.TexasHoldem

object Solver {

  def process(line: String): String = {

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => TexasHoldem.getAnswer(board, hands)
      case "omaha-holdem" :: board :: hands => "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => "The solution doesn't support Five Card Draw"
      case x :: _ => "Unrecognized game type"
      case _ => "Invalid input"
    }
  }
}
