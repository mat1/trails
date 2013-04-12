package ch.fhnw.imvs.trails

import scala.language.implicitConversions
import scala.annotation.tailrec

/** trails provides purely functional graph traverser combinators. */
trait Trails { self =>

  /** Type of the environment available to every Traverser. */
  type Environment

  /** Type of a single element in the graph. Typically a common super type of Vertex and Edge. */
  type PathElement

  type Path = List[PathElement]

  final case class State[+Head <: PathElement](path: Path)

  type Traverser[-I,+O,+A] = Environment => I => Stream[(O,A)]

  object Traverser {
    def run[E <: PathElement,A](tr: Traverser[State[Nothing],State[E],A], env: Environment): Stream[(Path,A)] =
      tr(env)(State[Nothing](Nil)).map { case (s,a) => (s.path.reverse, a) }
  }

  /** Provides some nice infix syntax. */
  final implicit class Syntax[I,O,A](t1: Traverser[I,O,A]) {
    def ~[P,B](t2: Traverser[O,P,B]): Traverser[I,P,A~B] = self.seq(t1, t2)
    def ~>[P,B](t2: Traverser[O,P,B]): Traverser[I,P,B] = self.map(seq(t1, t2)){ case a ~ b => b }
    def <~[P,B](t2: Traverser[O,P,B]): Traverser[I,P,A] = self.map(seq(t1, t2)){ case a ~ b => a }
    def |(t2: => Traverser[I,O,A]): Traverser[I,O,A] = self.choice(t1, t2)

    def ^^[B](f: A => B): Traverser[I,O,B] = self.map(t1)(f)

    def flatMap[P,B](f: A => Traverser[O,P,B]): Traverser[I,P,B] = self.flatMap(t1)(f)
    def map[B](f: A => B): Traverser[I,O,B] = self.map(t1)(f)
    def filter(p: A => Boolean): Traverser[I,O,A] = self.filter(t1)(p)
    def withFilter(p: A => Boolean): TraverserWithFilter[I,O,A] = new TraverserWithFilter(t1, p)
    final class TraverserWithFilter[-I,+O,+A](tr: Traverser[I,O,A], p: A => Boolean) {
      def map[B](f: A => B): Traverser[I,O,B] = self.map(self.filter(tr)(p))(f)
      def flatMap[P,B](f: A => Traverser[O,P,B]): Traverser[I,P,B] = self.flatMap(self.filter(tr)(p))(f)
      def withFilter(q: A => Boolean): TraverserWithFilter[I,O,A] = new TraverserWithFilter[I,O,A](tr, x => p(x) && q(x))
    }
  }

  final implicit class Syntax2[S,A](t1: Traverser[S,S,A]) {
    def ? : Traverser[S,S,Option[A]] = self.opt(t1)
    def * : Traverser[S,S,Stream[A]] = self.many(t1)
    def + : Traverser[S,S,Stream[A]] = self.many1(t1)
  }

  final case class ~[+A,+B](a: A, b: B) {
    override def toString: String = s"$a ~ $b"
  } // Product
  sealed trait <|>[+A,+B] // Sum
  final case class <|[A](a: A) extends <|>[A,Nothing]
  final case class |>[B](a: B) extends <|>[Nothing,B]

  /* Monadic API. */
  final def flatMap[I,M,O,A,B](tr: Traverser[I,M,A])(f: A => Traverser[M,O,B]): Traverser[I,O,B] =
    env => st0 => tr(env)(st0).flatMap { case (st1,a) => f(a)(env)(st1) }

  /** Returns a traverser which returns its input as the output.
    * @return a traverser which returns its input
    */
  final def success[S,A](a: A): Traverser[S,S,A] =
    _ => st => Stream((st,a))

  /* MonadPlus addons. */

  /** Returns the 'parallel' composition of the two given traversers which follows both alternatives.
    * @param either one of the traverser to follow
    * @param or one of the traverser to follow
    * @return the parallel composition of the two given traversers
    */
  final def choice[I,O,A](either: Traverser[I,O,A], or: => Traverser[I,O,A]): Traverser[I,O,A] =
    env => st => either(env)(st) #::: or(env)(st)

  /** Returns a traverser which drops its input and returns a empty output.
    * @return a traverser which drops its input
    */
  final def fail[X,A]: Traverser[X,X,A] =
    _ => _ => Stream()

  /* Environment access and State read and write */
  final def getEnv[S]: Traverser[S,S,Environment] =
    env => st => Stream((st, env))

  final def getState[S]: Traverser[S,S,S] =
    _ => st => Stream((st, st))

  final def setState[I,O](st: O): Traverser[I,O,Unit] =
    _ => _ => Stream((st, ()))

  //
  final def updateState[I,O](f: I => O): Traverser[I,O,Unit] =
    flatMap(getState[I])(st => setState[I,O](f(st)))

  // Every monad is a functor
  final def map[I,O,A,B](tr: Traverser[I,O,A])(f: A => B): Traverser[I,O,B] =
    flatMap(tr)(a => success[O,B](f(a)))

  // filter can be implemented for any monad plus instance
  final def filter[I,O,A](tr: Traverser[I,O,A])(f: A => Boolean): Traverser[I,O,A] =
    flatMap(tr)(a => if(f(a)) success[O,A](a) else fail[O,A])


  /** Returns the sequential composition of fst and snd which first follows the fst traverser
    * and then feeds the results into the snd traverser.
    * @param fst the first traverser to apply
    * @param snd the subsequent traverser to apply
    * @return the sequential composition of fst and snd
    */
  final def seq[I,M,O,A,B](fst: Traverser[I,M,A], snd: Traverser[M,O,B]): Traverser[I,O,A~B] =
    flatMap(fst)(a => map(snd)(b => new ~(a,b)))

  /*
      env => s0 => {
      (map[A,A|B](either) { a => new <|(a) }(env)(s0)) #::: map[B,A|B](or) { a => new |>(a) }(env)(s0)
    }
   */

  /** Returns a traverser which optionally follows the given traverser.
    * @param tr the traverser to follow optionally
    * @return a traverser which optionally follows the given traverser
    */
  final def opt[S,A](tr: Traverser[S,S,A]): Traverser[S,S,Option[A]] =
    choice(map(success[S,Unit](()))(_ => None), map(tr)(Some[A](_)))

  /** Returns a traverser which repeats the given traverser 0..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverserN
    */
  final def many[S,A](tr: Traverser[S,S,A]): Traverser[S,S,Stream[A]] =
    choice(success[S,Stream[Nothing]](Stream()),many1(tr))

  /*
    final def many[A](tr: Traverser[A]): Traverser[Stream[A]]=
      map(choice(success(Stream()),internal_many1(tr))){
        case <|(a) => a
        case |>(a) => a
      }
   */

  /** Returns a traverser which repeats the given traverser 1..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverser
    */
  final def many1[S,A](tr: Traverser[S,S,A]): Traverser[S,S,Stream[A]] =
    flatMap(tr)(a => map(many(tr))( as => a #:: as))
}



