package ch.fhnw.imvs.trails.parser

import org.scalatest.FunSuite
import TrailsParser._

class TrailsParserTest extends FunSuite {
  test("item") {
    def applyItem(s: String): Stream[(State,Char)] =
      item(())(State(s.toList))

    assert(applyItem("abc") contains (State("bc".toList), 'a'))
    assert(applyItem("abc").size === 1)
    assert(applyItem("").isEmpty)
  }

  test("email case") {
    case class Email(name: String, domain: String, topLevel: String)

    val domainChars = alphanum | char('_') | char('-')
    val nameChars: Traverser[Char] = domainChars | char('.')
    val email = nameChars.+ ~ char('@') ~ domainChars.+ ~ char('.') ~ letter.+ ^^ { case name ~ _ ~ domain ~ _ ~ top =>
      Email(name.mkString, domain.mkString, top.mkString)
    }

    def applyEmail(s: String): Stream[(State,Email)] = email(())(State(s.toList)).filter{ case (State(rest),_) => rest.isEmpty}

    assert(applyEmail("daniel.kroeni@fhnw.ch").size === 1)
    assert(applyEmail("daniel.kroeni@fhnw.ch") contains ((State(Nil), Email("daniel.kroeni", "fhnw", "ch"))))
  }
}
