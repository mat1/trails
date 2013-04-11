package ch.fhnw.imvs.trails.parser

import ch.fhnw.imvs.trails.Trails


object TrailsParser extends Trails {
  type Environment = Unit
  type PathElement = Char

  type Parser[A] = Traverser[Char,Char,A]

  def item: Parser[Char] =
    Traverser(env => state => state match {
      case State(c :: cs) => Stream((State(cs),c))
      case State(Nil) => Stream()
    })
   /* for {
      s <- getState if s.path.nonEmpty
      _ <- setState(s.copy(path = s.path.tail))
    } yield s.path.head */

  def sat(p: Char => Boolean): Parser[Char] = filter(item)(p)

  def char(c: Char): Parser[Char] = sat(_ == c)
  def digit: Parser[Char] = sat(_.isDigit)
  def letter: Parser[Char] = sat(_.isLetter)
  def alphanum: Parser[Char] = digit | letter
}


