import Solver.process

import java.io.PrintWriter
import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val sourceInput = Source.fromFile("C:/Users/Koccm/IdeaProjects/myPoker/src/main/scala/input/myInput.txt")
    val linesInput = sourceInput.getLines().toList

    val sourceAns = Source.fromFile("C:/Users/Koccm/IdeaProjects/myPoker/src/main/scala/output/output.txt")
    val lineAnswer = sourceAns.getLines().toList

    val programOutput = linesInput.map(line => process(line))


    def findNotEqualAnswer(answerList: List[String], programAnswerList: List[String]) = {

      @tailrec
      def helper(answerList: List[String], progAnswer: List[String], accList: List[String], numberOfLine: Int): List[String] = {

        val newNumberOfLine: Int = answerList match {
          case _ :: _ => numberOfLine + 1
          case Nil => numberOfLine
        }

        val newAccList: List[String] = answerList match {
          case head :: _ if head.equals(progAnswer.head)  => List.empty
          case head :: _ if !head.equals(progAnswer.head) => accList :+ (head ++ s" ->  ${newNumberOfLine.toString} line number.")
          case _ => accList
        }

        answerList match {
          case _ :: tail => helper(tail, progAnswer.tail, newAccList, newNumberOfLine)
          case _ => newAccList
        }
      }
      helper(answerList, programAnswerList, List.empty, 0)
    }

    val ans = if (findNotEqualAnswer(lineAnswer, programOutput).isEmpty && lineAnswer.length == programOutput.length) List("Answer Good")
      else {
        if (lineAnswer.length != programOutput.length) List("Length input not similar with output")
        else findNotEqualAnswer(lineAnswer, programOutput)
    }


    ans.foreach(println)

  }

}
