package com.mypoker

import com.mypoker.domain.Rank._
import com.mypoker.domain.Suit._

object Myyyy extends App {

  val ok = List("4s8d", "Qh9s")
  println(ok.flatMap(_.grouped(2).toList))

  println("a2k3".grouped(2).toList)
}
