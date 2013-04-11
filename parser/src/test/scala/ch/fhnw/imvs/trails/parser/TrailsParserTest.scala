package ch.fhnw.imvs.trails.parser
/*
import org.scalatest.FunSuite
import TrailsParser._
// test-only ch.fhnw.imvs.trails.parser.TrailsParserTest
class TrailsParserTest extends FunSuite {
  test("item") {
    def applyItem(s: String): Stream[(State,Char)] =
      item(())(State(s.toList, None))

    assert(applyItem("abc") contains ((State("bc".toList, None), 'a')))
    assert(applyItem("abc").size === 1)
    assert(applyItem("").isEmpty)
  }

  test("cycles?") {
    val g = item.+

    println(g(())(State("ab".toList, None)).map(p => (p._1, p._2.toList)).toList)

  }
}
*/
