package ch.fhnw.imvs.trails

trait Trails {
  type Environment
  type PathElement

  type Path = List[PathElement]

  case class Trace(path: Path, visitedPaths: Option[Set[Path]])

  type Traverser = Environment => Trace => Stream[Trace]

  // Generic Combinators
  def seq(fst: Traverser, snd: Traverser): Traverser =
    env => t => fst(env)(t).flatMap(snd(env))

  def choice(either: Traverser, or: Traverser): Traverser =
    env => t => either(env)(t) ++ or(env)(t)

  def optional(tr: Traverser): Traverser =
    choice(accept, tr)

  def many(tr: Traverser): Traverser =
    withCycleDetection(internal_many(tr))

  def many1(tr: Traverser): Traverser =
    withCycleDetection(internal_many1(tr))

  private def withCycleDetection(tr: Traverser): Traverser = {
    env => t =>
      val currentVisitedEdges = t.visitedPaths
      val res = tr(env)(t.copy(visitedPaths = Some(Set())))
      res.map(_.copy(visitedPaths = currentVisitedEdges)) // restore edge Set
  }

  private def internal_many1(tr: Traverser): Traverser = {
    env => t => {
      val size = t.path.size
      tr(env)(t).flatMap { case Trace(path, Some(visitedPaths)) =>
        val currentEvaluation = path.take(path.size - size)
        if (visitedPaths(currentEvaluation)) Stream()   // println("Found Cycle: Repeating pattern" + currentEvaluation.reverse.map(format) + " Current set: " + visited.map(_.reverse.map(format)) + " base trace: " + t)
        else internal_many(tr)(env)(Trace(path, Some(visitedPaths + currentEvaluation)))
      }
    }
  }

  private def internal_many(tr: Traverser): Traverser =
    choice(accept, internal_many1(tr))

  def accept: Traverser =
    _ => t => Stream(t)

  def fail: Traverser =
    _ => _ => Stream()

  def filter(p: Trace => Boolean): Traverser =
    env => t => if(p(t)) Stream(t) else Stream()

  def filterHead(p: PathElement => Boolean): Traverser =
    filter(t => t match { case Trace(head :: rest, _) => p(head)} )

  // Infix syntax
  implicit class Syntax(t1: Traverser) {
    def ~(t2: Traverser): Traverser = seq(t1, t2)
    def |(t2: Traverser): Traverser = choice(t1, t2)
    def ? : Traverser = optional(t1)
    def * : Traverser = many(t1)
    def + : Traverser = many1(t1)

    def run(s: Environment) = t1(s)(Trace(Nil, None))
  }
}


