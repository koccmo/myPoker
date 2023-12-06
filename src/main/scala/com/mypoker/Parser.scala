package com.mypoker

import com.mypoker.domain.Hand
import scala.annotation.tailrec

trait Parse {
  def apply(hands: List[Hand]): String
}

object Parse {

  def apply(): Parse = new Parse {
    def apply(hands: List[Hand]): String = {
      @tailrec
      def helper(hands: List[Hand], accAns: String, accValue: Int): String = {
        hands match {
          case head :: tail
            if accValue == 0 => helper(tail, accAns + s"${head.toString}", head.strength.getOrElse(0))
          case head :: tail
            if accValue == head.strength.getOrElse(0) => helper(tail, accAns + s"=${head.toString}", head.strength.getOrElse(0))
          case head :: tail
            if accValue != head.strength.getOrElse(0) => helper(tail, accAns + s" ${head.toString}", head.strength.getOrElse(0))
          case _ => accAns
        }
      }

      helper(hands, "", 0)
    }
  }
}
