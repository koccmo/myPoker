package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CalculatorSpec extends AnyFunSuite with Matchers{

  test("add"){
    val result = 2 + 3
    result.shouldEqual(5)
  }
}
