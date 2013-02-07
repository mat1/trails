package ch.fhnw.imvs.trails

import scalaz.Show

/** trails provides purely functional graph traverser combinators. */
trait Trails {

  /** Type of the environment available to every Traverser. */
  type Environment

  /** Type of a single element in the graph. Typically a common super type of Vertex and Edge. */
  type PathElement

  /** Type of a path through the graph. */
  type Path = List[PathElement]

  /** Also a path through the graph but with additional bookkeeping for named sub-paths and cycle detection. */
  case class Trace(path: Path, visitedPaths: Option[Set[Path]])

  type State

  /** A Traverser is a function which takes an Environment (we may use a Reader monad),
    * an input Trace and produces a Stream of subsequent Traces.
    * Traverser is THE compositional unit in trails.
    */
  type Traverser = Environment => Pair[Trace, State] => Stream[Pair[Trace, State]]



  /** Returns the sequential composition of fst and snd which first follows the fst traverser
    * and then feeds the results into the snd traverser.
    * @param fst the first traverser to apply
    * @param snd the subsequent traverser to apply
    * @return the sequential composition of fst and snd
    */
  def seq(fst: Traverser, snd: Traverser): Traverser =
    env => t => fst(env)(t).flatMap(snd(env))

  /** Returns the 'parallel' composition of the two given traversers which follows both alternatives.
    * @param either one of the traverser to follow
    * @param or one of the traverser to follow
    * @return the parallel composition of the two given traversers
    */
  def choice(either: Traverser, or: Traverser): Traverser =
    env => t => either(env)(t) ++ or(env)(t)

  /** Returns a traverser which optionally follows the given traverser.
    * @param tr the traverser to follow optionally
    * @return a traverser which optionally follows the given traverser
    */
  def optional(tr: Traverser): Traverser =
    choice(accept, tr)

  /** Returns a traverser which repeats the given traverser 0..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverser
    */
  def many(tr: Traverser): Traverser =
    withCycleDetection(internal_many(tr))

  /** Returns a traverser which repeats the given traverser 1..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverser
    */
  def many1(tr: Traverser): Traverser =
    withCycleDetection(internal_many1(tr))

  private def withCycleDetection(tr: Traverser): Traverser = {
    env => ts =>
      val (trace, state) = ts
      // store the set of already visited paths
      val currentVisitedEdges = trace.visitedPaths
      // run the given traverser within a new context
      val res = tr(env)((trace.copy(visitedPaths = Some(Set())),state))
      // restore the set of already visited paths
      res.map { case (t, s) => (t.copy(visitedPaths = currentVisitedEdges), s) }
  }

  private def internal_many(tr: Traverser): Traverser =
    choice(accept, internal_many1(tr))

  private def internal_many1(tr: Traverser): Traverser =
    env => ts => {
      val (trace, state) = ts
      val size = trace.path.size
      tr(env)(ts).flatMap { case (Trace(path, Some(visitedPaths)), state) =>
        val currentEvaluation = path.take(path.size - size)
        if (visitedPaths(currentEvaluation)) Stream()   // println("Found Cycle: Repeating pattern" + currentEvaluation.reverse.map(format) + " Current set: " + visited.map(_.reverse.map(format)) + " base trace: " + t)
        else internal_many(tr)(env)((Trace(path, Some(visitedPaths + currentEvaluation)), state))
      }
    }


  /** Returns a traverser which returns its input as the output.
    * @return a traverser which returns its input
    */
  def accept: Traverser =
    _ => t => Stream(t)

  /** Returns a traverser which drops its input and returns a empty output.
    * @return a traverser which drops its input
    */
  def fail: Traverser =
    _ => _ => Stream()

  /** Returns a traverser which filters its input trace using the given predicate.
    * @param p the predicate
    * @return a traverser which filters its input trace
    */
  def filter(p: Pair[Trace,State] => Boolean): Traverser =
    env => t => if(p(t)) Stream(t) else Stream()

  /** Returns a traverser which filters the head of its input trace using the given predicate.
    * @param p the predicate
    * @return a traverser which filters the head of its input trace
    */
  def filterHead(p: PathElement => Boolean): Traverser =
    filter(t => t match { case (Trace(head :: rest, _), state) => p(head)} )



  /** Provides some nice infix syntax. */
  implicit class Syntax(t1: Traverser) {
    def ~(t2: Traverser): Traverser = seq(t1, t2)
    def |(t2: Traverser): Traverser = choice(t1, t2)
    def ? : Traverser = optional(t1)
    def * : Traverser = many(t1)
    def + : Traverser = many1(t1)

    def run(e: Environment, s: State) = t1(e)((Trace(Nil, None),s))
  }

  // Show instances
  implicit def showPath(implicit showPathElem: Show[PathElement]): Show[Path]  = new Show[Path] {
    override def shows(p: Path): String = p.reverse.map(showPathElem.show).mkString("["," ","]")
  }

  implicit def showTrace(implicit showPathElem: Show[PathElement]): Show[Trace] = new Show[Trace] {
    override def shows(t: Trace): String = s"Trace(${Show[Path].shows(t.path)})"
  }
}


