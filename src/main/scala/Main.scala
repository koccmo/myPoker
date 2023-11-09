import Solver.process

import java.io.PrintWriter
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("C:/Users/Koccm/IdeaProjects/myPoker/src/main/scala/input/myInput.txt")
    val lines = source.getLines().toList
    source.close()

    val processedData = lines.map(line => process(line))

    val output = new PrintWriter("answer.txt")
    processedData.foreach(output.println)
    output.close()

    println("Data processing and writing to answer.txt completed.")
  }

}
