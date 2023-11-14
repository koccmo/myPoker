import cards.{Card, Rank, Suit}

object okkk extends App {

  val myString: List[String] = List("KhQsKhQs")
  val string1 = myString.map(_.split("(?<=\\G..)"))
  val string2 = myString.map(_.replace("\\s", ""))

  val check = myString.map(_.split("(?<=\\G..)"))
  val check2 = check.flatMap(_.zipWithIndex)
  val check3 = check.flatMap(_.flatMap(_.zipWithIndex)).map{
    case (char, int) if int == 0 => Rank.fromString(char.toString)
  }

  check.flatten.foreach(println)
  println(check2)
  println(check3)

  println("-----------------")


}
