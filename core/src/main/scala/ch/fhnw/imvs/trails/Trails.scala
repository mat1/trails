package ch.fhnw.imvs.trails

import scalaz.Show
import scalaz.std.option._

/** trails provides purely functional graph traverser combinators. */
trait Trails {

  /** Type of the environment available to every Traverser. */
  type Environment

  /** Type of a single element in the graph. Typically a common super type of Vertex and Edge. */
  type PathElement

  /** Type of a path through the graph. */
  type Path = List[PathElement]

  // Extending this state by override
  type StateExtension = Unit

  /** Type of the state which is passed along with the Trace */
  type State = ((Path, Option[Set[Path]]),StateExtension)

  /** A Traverser is a function which takes an Environment (we may use a Reader monad),
    * an input Trace and produces a Stream of subsequent Traces.
    * Traverser is THE compositional unit in trails.
    */
  type Traverser[+A] = Environment => State => Stream[(State,A)]

  final case class ~[+A,+B](a: A, b: B) // Product
  sealed trait |[+A,+B] // Sum
  final case class <|[A](a: A) extends |[A,Nothing]
  final case class |>[B](a: B) extends |[Nothing,B]

  /** Returns the sequential composition of fst and snd which first follows the fst traverser
    * and then feeds the results into the snd traverser.
    * @param fst the first traverser to apply
    * @param snd the subsequent traverser to apply
    * @return the sequential composition of fst and snd
    */
  def product[A,B](fst: Traverser[A], snd: Traverser[B]): Traverser[A~B] =
    env => s0 => for { (s1,a) <- fst(env)(s0); (s2,b) <- snd(env)(s1) } yield (s2, new ~(a,b))

  /** Returns the 'parallel' composition of the two given traversers which follows both alternatives.
    * @param either one of the traverser to follow
    * @param or one of the traverser to follow
    * @return the parallel composition of the two given traversers
    */
  def choice[A,B](either: Traverser[A], or: => /* this is important */ Traverser[A]): Traverser[A] =
    env => s0 => either(env)(s0) #::: (or(env)(s0))
  /*
      env => s0 => {
      (map[A,A|B](either) { a => new <|(a) }(env)(s0)) #::: map[B,A|B](or) { a => new |>(a) }(env)(s0)
    }
   */

  /** Returns a traverser which optionally follows the given traverser.
    * @param tr the traverser to follow optionally
    * @return a traverser which optionally follows the given traverser
    */
  def optional[A](tr: Traverser[A]): Traverser[Option[A]] =
    choice(map(success(()))(_ => none[A]), map(tr)(some[A]))

  def slice[A](tr: Traverser[A]): Traverser[(Path,A)] =
    env => st => {
    val ((path, _), _) = st
    val size = path.size
    tr(env)(st).map { case (((path, visitedPaths), state),a) =>
      (((path, visitedPaths), state), (path.take(path.size - size), a))
    }
  }

  /** Returns a traverser which repeats the given traverser 0..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverserN
    */
  def many[A](tr: Traverser[A]): Traverser[Stream[A]] =
    withCycleDetection(internal_many(tr))

  def internal_many[A](tr: Traverser[A]): Traverser[Stream[A]] =
    choice(success(Stream()),internal_many1(tr))

  /*
    def internal_many[A](tr: Traverser[A]): Traverser[Stream[A]] =
    map(choice(success(Stream()),internal_many1(tr))){
      case <|(a) => a
      case |>(a) => a
    }

   */

  /** Returns a traverser which repeats the given traverser 1..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverser
    */
  def many1[A](tr: Traverser[A]): Traverser[Stream[A]] =
    withCycleDetection(internal_many1(tr))
    //map(product(tr, many(tr))){ case a ~ b => a #:: b }

  private def internal_many1[A](tr: Traverser[A]): Traverser[Stream[A]] =
    env => st => {
      slice(tr)(env)(st).flatMap { case (((path, Some(visitedPaths)), state),(slice, v)) =>
        if (visitedPaths(slice)) Stream()   // println("Found Cycle: Repeating pattern" + currentEvaluation.reverse.map(format) + " Current set: " + visited.map(_.reverse.map(format)) + " base trace: " + t)
        else map(internal_many(tr)) { vs => v #:: vs }(env)(((path, Some(visitedPaths + slice)), state))
      }

      /*val ((path, _), _) = st
      val size = path.size
      tr(env)(st).flatMap { case (((path, Some(visitedPaths)), state),v) =>
        val currentEvaluation = path.take(path.size - size)
        if (visitedPaths(currentEvaluation)) Stream()   // println("Found Cycle: Repeating pattern" + currentEvaluation.reverse.map(format) + " Current set: " + visited.map(_.reverse.map(format)) + " base trace: " + t)
        else map(internal_many(tr)) { vs => v #:: vs }(env)(((path, Some(visitedPaths + currentEvaluation)), state))
      }*/
    }


  private def withCycleDetection[A](tr: Traverser[A]): Traverser[A] = {
    env => ts =>
      val ((path, visitedPaths), e) = ts
      // store the set of already visited paths
      val currentVisitedEdges = visitedPaths
      // run the given traverser within a new context
      val res = tr(env)(((path, Some(Set())),e))
      // restore the set of already visited paths
      res.map { case (((t,_), s),v) => (((t,currentVisitedEdges), s),v) }
  }


  def map[A,B](tr: Traverser[A])(f: A => B): Traverser[B] =
    env => ts => tr(env)(ts).map{ case (s,a) => (s,f(a)) }

  def map2[A,B,C](ta: Traverser[A], tb: Traverser[B])(f: (A,B) => C): Traverser[C] = null

  /** Returns a traverser which returns its input as the output.
    * @return a traverser which returns its input
    */
  def success[A](a: A): Traverser[A] =
    _ => s0 => Stream((s0,a))

  /** Returns a traverser which drops its input and returns a empty output.
    * @return a traverser which drops its input
    */
  def fail[A]: Traverser[A] =
    _ => _ => Stream()

  /** Provides some nice infix syntax. */
  implicit class Syntax[A](t1: Traverser[A]) {
    def ~[B](t2: Traverser[B]): Traverser[A~B] = product(t1, t2)
    def ~>[B](t2: Traverser[B]): Traverser[B] = map(product(t1, t2)){ case a ~ b => b }
    def <~[B](t2: Traverser[B]): Traverser[A] = map(product(t1, t2)){ case a ~ b => a }
    def |(t2: Traverser[A]): Traverser[A] = choice(t1, t2)
    def ? : Traverser[Option[A]] = optional(t1)
    def * : Traverser[Stream[A]] = many(t1)
    def + : Traverser[Stream[A]] = many1(t1)

    def ^^[B](f: A => B): Traverser[B] = map(t1)(f)

    def run(e: Environment, s: StateExtension) = t1(e)(((Nil, None),s))
  }

  // Show instances
  implicit def showPath(implicit showPathElem: Show[PathElement]): Show[Path]  = new Show[Path] {
    override def shows(p: Path): String = p.reverse.map(showPathElem.show).mkString("["," ","]")
  }
}


