package com.mypoker.domain

import com.mypoker.validation.ValidationError

sealed trait Rank {
  def strength: Int
}

object Rank {

  case object Two extends Rank {
    override def toString: String = "2"

    override def strength: Int = 1
  }

  case object Three extends Rank {
    override def toString: String = "3"

    override def strength: Int = 2
  }

  case object Four extends Rank {
    override def toString: String = "4"

    override def strength: Int = 3
  }

  case object Five extends Rank {
    override def toString: String = "5"

    override def strength: Int = 4
  }

  case object Six extends Rank {
    override def toString: String = "6"

    override def strength: Int = 5
  }

  case object Seven extends Rank {
    override def toString: String = "7"

    override def strength: Int = 6
  }

  case object Eight extends Rank {
    override def toString: String = "8"

    override def strength: Int = 7
  }

  case object Nine extends Rank {
    override def toString: String = "9"

    override def strength: Int = 8
  }

  case object Ten extends Rank {
    override def toString: String = "T"

    override def strength: Int = 9
  }

  case object Jack extends Rank {
    override def toString: String = "J"

    override def strength: Int = 10
  }

  case object Queen extends Rank {
    override def toString: String = "Q"

    override def strength: Int = 11
  }

  case object King extends Rank {
    override def toString: String = "K"

    override def strength: Int = 12
  }

  case object Ace extends Rank {
    override def toString: String = "A"

    override def strength: Int = 13
  }

  def fromString(s: String): Either[ValidationError, Rank] =
    s match {
      case "2" => Right(Two)
      case "3" => Right(Three)
      case "4" => Right(Four)
      case "5" => Right(Five)
      case "6" => Right(Six)
      case "7" => Right(Seven)
      case "8" => Right(Eight)
      case "9" => Right(Nine)
      case "T" => Right(Ten)
      case "J" => Right(Jack)
      case "Q" => Right(Queen)
      case "K" => Right(King)
      case "A" => Right(Ace)
      case _   => Left(ValidationError.IncorrectRank(s))
    }
}
