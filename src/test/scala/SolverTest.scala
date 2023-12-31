import com.mypoker.services.{CalculateStrength, Parse, ProcessResult}

import com.mypoker.services.validation.Validate
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.{BufferedSource, Source}

class SolverTest extends AnyFunSuite with Matchers {

  val validate: Validate = Validate()
  val parse: Parse = Parse()
  val calculateStrength: CalculateStrength = CalculateStrength()
  val process: ProcessResult = ProcessResult(validate, calculateStrength, parse)

  test("Texas-Holdem test with inputs") {
    val sourceInput: BufferedSource = Source.fromFile("src/main/resources/myInput.txt")
    val linesInput: List[String] = sourceInput.getLines().toList

    val sourceAns: BufferedSource = Source.fromFile("src/main/resources/output.txt")
    val lineAnswer: List[String] = sourceAns.getLines().toList

    val programOutput: List[String] = linesInput.map(process.apply)

    programOutput shouldEqual lineAnswer
  }
}
