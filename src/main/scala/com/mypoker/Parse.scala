package com.mypoker

import com.mypoker.domain.Hand

import scala.annotation.tailrec

trait Parse {

  def apply(hands: List[Hand]): String
}

object Parse {

  def apply(): Parse =
    new Parse {

      def apply(hands: List[Hand]): String = {
        @tailrec
        def helper(
          hands: List[Hand],
          result: String,
          previousValue: Int
        ): String =
          hands match {
            case hand :: other if previousValue == 0                          =>
              helper(other, result + s"${hand.toString}", hand.strength.getOrElse(0))
            case hand :: other if previousValue == hand.strength.getOrElse(0) =>
              helper(other, result + s"=${hand.toString}", hand.strength.getOrElse(0))
            case hand :: other if previousValue != hand.strength.getOrElse(0) =>
              helper(other, result + s" ${hand.toString}", hand.strength.getOrElse(0))
            case _                                                      => result
          }

        helper(hands, "", 0)
      }
    }
}
