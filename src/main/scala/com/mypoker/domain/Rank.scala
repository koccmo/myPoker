package com.mypoker.domain

sealed trait Rank {
  def strength: Int
}

object Rank {

  case object Two extends Rank {
    override def toString: String = "2"
    def strength: Int = 1
  }

  case object Three extends Rank {
    override def toString: String = "3"
    def strength: Int = 2
  }

  case object Four extends Rank {
    override def toString: String = "4"
    def strength: Int = 3
  }

  case object Five extends Rank {
    override def toString: String = "5"
    def strength: Int = 4
  }

  case object Six extends Rank {
    override def toString: String = "6"
    def strength: Int = 5
  }

  case object Seven extends Rank {
    override def toString: String = "7"
    def strength: Int = 6
  }

  case object Eight extends Rank {
    override def toString: String = "8"
    def strength: Int = 7
  }

  case object Nine extends Rank {
    override def toString: String = "9"
    def strength: Int = 8
  }

  case object Ten extends Rank {
    override def toString: String = "T"
    def strength: Int = 9
  }

  case object Jack extends Rank {
    override def toString: String = "J"
    def strength: Int = 10
  }

  case object Queen extends Rank {
    override def toString: String = "Q"
    def strength: Int = 11
  }

  case object King extends Rank {
    override def toString: String = "K"
    def strength: Int = 12
  }

  case object Ace extends Rank {
    override def toString: String = "A"
    def strength: Int = 13
  }

  val ValuesList: List[Rank] = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  val RankValuesMap: Map[String, Rank] = ValuesList.map(rank => (rank.toString, rank)).toMap
}
