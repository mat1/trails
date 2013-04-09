package ch.fhnw.imvs.trails.parser

import ch.fhnw.imvs.trails.Trails


object TrailsParser extends Trails {
  type Environment = Unit
  type PathElement = Char

  def item: Traverser[Char] =
    for {
      State(chars) <- getState if chars.nonEmpty
      _ <- setState(State(chars.tail))
    } yield chars.head

  def sat(p: Char => Boolean): Traverser[Char] =
    filter(item)(p)

  def char(c: Char): Traverser[Char] =
    sat(_ == c)

  def digit: Traverser[Char] =
    sat(_.isDigit)
}
