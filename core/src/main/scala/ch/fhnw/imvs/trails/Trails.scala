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
  case class Trace(path: Path, namedSubPaths: Map[String, List[Path]], visitedPaths: Option[Set[Path]])

  /** A Traverser is a function which takes an Environment (we may use a Reader monad),
    * an input Trace and produces a Stream of subsequent Traces.
    * Traverser is THE compositional unit in trails.
    */
  type Traverser = Environment => Trace => Stream[Trace]



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
    env => t =>
      // store the set of already visited paths
      val currentVisitedEdges = t.visitedPaths
      // run the given traverser within a new context
      val res = tr(env)(t.copy(visitedPaths = Some(Set())))
      // restore the set of already visited paths
      res.map(_.copy(visitedPaths = currentVisitedEdges))
  }

  private def internal_many(tr: Traverser): Traverser =
    choice(accept, internal_many1(tr))

  private def internal_many1(tr: Traverser): Traverser = {
    env => t => {
      val size = t.path.size
      tr(env)(t).flatMap { case Trace(path, namedSubPaths, Some(visitedPaths)) =>
        val currentEvaluation = path.take(path.size - size)
        if (visitedPaths(currentEvaluation)) Stream()   // println("Found Cycle: Repeating pattern" + currentEvaluation.reverse.map(format) + " Current set: " + visited.map(_.reverse.map(format)) + " base trace: " + t)
        else internal_many(tr)(env)(Trace(path, namedSubPaths, Some(visitedPaths + currentEvaluation)))
      }
    }
  }


  /** Returns a traverser which stores the generated sub-paths in a map with the given name as the key.
    * @param name the name under which the sub-path is stored in the map
    * @param tr the traverser
    * @return a traverser which stores the generated sub-paths in a map
    */
  def name(name: String, tr: Traverser): Traverser =
    env => t => {
      val size = t.path.size
      tr(env)(t).map { case Trace(path, namedSubPaths, visitedPaths) =>
        val currentEvaluation = path.take(path.size - size)
        Trace(path, namedSubPaths.updated(name, currentEvaluation :: namedSubPaths.getOrElse(name, Nil)), visitedPaths)
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
  def filter(p: Trace => Boolean): Traverser =
    env => t => if(p(t)) Stream(t) else Stream()

  /** Returns a traverser which filters the head of its input trace using the given predicate.
    * @param p the predicate
    * @return a traverser which filters the head of its input trace
    */
  def filterHead(p: PathElement => Boolean): Traverser =
    filter(t => t match { case Trace(head :: rest, _, _) => p(head)} )



  /** Provides some nice infix syntax. */
  implicit class Syntax(t1: Traverser) {
    def ~(t2: Traverser): Traverser = seq(t1, t2)
    def |(t2: Traverser): Traverser = choice(t1, t2)
    def ? : Traverser = optional(t1)
    def * : Traverser = many(t1)
    def + : Traverser = many1(t1)

    def as(n: String): Traverser = name(n, t1)

    def run(s: Environment) = t1(s)(Trace(Nil, Map(), None))
  }



  /**
   * A table representation of the named parts of a Stream of Traces.
   * @param traces the traces
   */
  case class Table(private val traces: Stream[Trace]) {
    val headers: Vector[String] = traces.foldLeft(Set[String]())((acc, t) => acc ++ t.namedSubPaths.keys).toVector.sorted
    def rows: Vector[Vector[List[Path]]] = traces.toVector.map(t => headers.map(h => t.namedSubPaths.getOrElse(h, Nil)))

    def pretty(implicit showPathElem: Show[PathElement]): String = {
      val colls = for((name, index) <- headers.zipWithIndex ) yield {
        val col = rows.map(row => row(index).map(Show[Path].shows).toString)
        val maxSize = col.map(_.size).max
        (name, maxSize, col)
      }

      val headerLine = colls.map { case (name, maxSize, _) => name.padTo(maxSize, " ").mkString }
      val data = for(i <- 0 until rows.size) yield {
        colls.map {case (name, maxSize, col) => col(i).padTo(maxSize, " ").mkString }
      }
      val separatorLine = colls.map { case (name, maxSize, _) => "-" * maxSize }
      (separatorLine +: headerLine +: separatorLine +: data :+ separatorLine).map(_.mkString("|","|","|")).mkString("\n")
    }
  }

  // Show instances
  implicit def showPath(implicit showPathElem: Show[PathElement]): Show[Path]  = new Show[Path] {
    override def shows(p: Path): String = p.reverse.map(showPathElem.show).mkString("["," ","]")
  }

  implicit def showTrace(implicit showPathElem: Show[PathElement]): Show[Trace] = new Show[Trace] {
    override def shows(t: Trace): String = s"Trace(${Show[Path].shows(t.path)})"
  }
}


