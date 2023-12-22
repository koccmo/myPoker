package com.mypoker.domain

sealed trait Suit

object Suit{

  val Suits: List[Suit] = List(Clubs, Diamonds, Hearts, Spades)
  val SuitsMap: Map[String, Suit] = Suits.map(suit => suit.toString -> suit).toMap

  case object Clubs extends Suit { override def toString: String = "c" }

  case object Diamonds extends Suit { override def toString: String = "d" }

  case object Hearts extends Suit { override def toString: String = "h" }

  case object Spades extends Suit { override def toString: String = "s" }
}