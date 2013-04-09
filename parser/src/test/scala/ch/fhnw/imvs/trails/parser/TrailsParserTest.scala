package ch.fhnw.imvs.trails.parser

import org.scalatest.FunSuite
import TrailsParser._

class TrailsParserTest extends FunSuite {
  test("really?") {
    val grammar = char('a') ~ digit.+ ~ (char('_') ~ digit.+).?

    val samples = Seq(
      "a1234",
      "b1234",
      "a_1",
      "a1234_123"
    ).map(s => State(s.toList))

    val prepared = grammar(())
    println(samples.map(prepared).map(_.force).mkString("\n"))
  }
}
