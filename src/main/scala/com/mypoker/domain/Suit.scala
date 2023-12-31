package com.mypoker.domain

sealed trait Suit

object Suit{

  case object Clubs extends Suit { override def toString: String = "c" }

  case object Diamonds extends Suit { override def toString: String = "d" }

  case object Hearts extends Suit { override def toString: String = "h" }

  case object Spades extends Suit { override def toString: String = "s" }

  val ValuesList: List[Suit] = List(Clubs, Diamonds, Hearts, Spades)
  val ValuesMap: Map[String, Suit] = ValuesList.map(suit => suit.toString -> suit).toMap
}