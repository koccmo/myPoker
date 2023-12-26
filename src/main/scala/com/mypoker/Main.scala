package com.mypoker

import com.mypoker.services.{CalculateStrength, Parse, ProcessResult}
import com.mypoker.services.validation.Validate

import scala.io.StdIn

object Main {

  val validate: Validate = Validate()
  val calculateStrength: CalculateStrength = CalculateStrength()
  val parse: Parse = Parse()
  val process: ProcessResult = ProcessResult(validate, calculateStrength, parse)

  def main(args: Array[String]): Unit = Iterator.continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x => x map process.apply foreach println }
}
