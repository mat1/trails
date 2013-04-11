package ch.fhnw.imvs.trails.parser
/*
import ch.fhnw.imvs.trails.Trails


object TrailsParser extends Trails {
  type Environment = Unit
  type PathElement = Char

  def item: Traverser[Char] =
    env => state => state match {
      case State(c :: cs, cy) => Stream((State(cs,cy),c))
      case State(Nil, cy) => Stream()
    }
   /* for {
      s <- getState if s.path.nonEmpty
      _ <- setState(s.copy(path = s.path.tail))
    } yield s.path.head */

  def sat(p: Char => Boolean): Traverser[Char] = filter(item)(p)

  def char(c: Char): Traverser[Char] = sat(_ == c)
  def digit: Traverser[Char] = sat(_.isDigit)
  def letter: Traverser[Char] = sat(_.isLetter)
  def alphanum: Traverser[Char] = digit | letter
}

*/
