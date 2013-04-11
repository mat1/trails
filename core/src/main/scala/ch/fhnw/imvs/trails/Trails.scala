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

  final case class State(path: Path, cycles: Option[Set[Path]])
  object State {
    val Empty = State(Nil,None)
  }

  trait Traverser[-In,+Out,+A] extends (Environment => State => Stream[(State,A)])

  object Traverser {
    def apply[In,Out,A](f: Environment => State => Stream[(State,A)]): Traverser[In,Out,A] = new Traverser[In,Out,A] {
      def apply(e: Environment): State => Stream[(State,A)] = f(e)
    }
    def run[IN,OUT,A](tr: Traverser[IN,OUT,A], env: Environment): Stream[(Path,A)] =
      tr(env)(State.Empty).map { case (s,a) => (s.path.reverse, a) }
  }

  /** Provides some nice infix syntax. */
  final implicit class Syntax[IN1,OUT1,A](t1: Traverser[IN1,OUT1,A]) {
    def ~[OUT2,B](t2: Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,A~B] = self.seq(t1, t2)
    def ~>[OUT2,B](t2: Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,B] = self.map(seq(t1, t2)){ case a ~ b => b }
    def <~[OUT2,B](t2: Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,A] = self.map(seq(t1, t2)){ case a ~ b => a }
    def |(t2: => Traverser[IN1,OUT1,A]): Traverser[IN1,OUT1,A] = self.choice(t1, t2)

    def ^^[B](f: A => B): Traverser[IN1,OUT1,B] = self.map(t1)(f)

    /* for-comprehension sugar */
    def flatMap[OUT2,B](f: A => Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,B] = self.flatMap(t1)(f)
    def map[B](f: A => B): Traverser[IN1,OUT1,B] = self.map(t1)(f)
    def filter(p: A => Boolean): Traverser[IN1,OUT1,A] = self.filter(t1)(p)
  }

  final implicit class Syntax2[IN1,A](t1: Traverser[IN1,IN1,A]) {
    def ? : Traverser[IN1,IN1,Option[A]] = self.opt(t1)
    def * : Traverser[IN1,IN1,Stream[A]] = self.many(t1)
    def + : Traverser[IN1,IN1,Stream[A]] = self.many1(t1)
  }

  final case class ~[+A,+B](a: A, b: B) {
    override def toString: String = s"$a ~ $b"
  } // Product
  sealed trait <|>[+A,+B] // Sum
  final case class <|[A](a: A) extends <|>[A,Nothing]
  final case class |>[B](a: B) extends <|>[Nothing,B]

  /* Monadic API. */
  final def flatMap[IN1,OUT1,A,OUT2,B](tr: Traverser[IN1,OUT1,A])(f: A => Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,B] =
    Traverser(env => st0 => tr(env)(st0).flatMap { case (st1,a) => f(a)(env)(st1) })

  /** Returns a traverser which returns its input as the output.
    * @return a traverser which returns its input
    */
  final def success[X,A](a: A): Traverser[X,X,A] =
    Traverser(_ => st => Stream((st,a)))

  /* MonadPlus addons. */

  /** Returns the 'parallel' composition of the two given traversers which follows both alternatives.
    * @param either one of the traverser to follow
    * @param or one of the traverser to follow
    * @return the parallel composition of the two given traversers
    */
  final def choice[IN,OUT,A](either: Traverser[IN,OUT,A], or: => Traverser[IN,OUT,A]): Traverser[IN,OUT,A] =
    Traverser(env => st => either(env)(st) #::: or(env)(st))

  /** Returns a traverser which drops its input and returns a empty output.
    * @return a traverser which drops its input
    */
  final def fail[IN,OUT,A]: Traverser[IN,OUT,A] =
    Traverser(_ => _ => Stream())

  /* Environment access and State read and write */
  final def getEnv[X]: Traverser[X,X,Environment] =
    Traverser(env => st => Stream((st, env)))

  final def getState[X]: Traverser[X,X,State] =
    Traverser(_ => st => Stream((st, st)))

  final def setState[IN,OUT](st: State): Traverser[IN,OUT,Unit] =
    Traverser(_ => _ => Stream((st, ())))

  //
  final def updateState[IN,OUT](f: State => State): Traverser[IN,OUT,Unit] =
    Traverser(flatMap(getState[IN])(st => setState[IN,OUT](f(st))))

  // Every monad is a functor
  final def map[IN,OUT,A,B](tr: Traverser[IN,OUT,A])(f: A => B): Traverser[IN,OUT,B] =
    Traverser(flatMap(tr)(a => success[OUT,B](f(a))))

  // filter can be implemented for any monad plus instance
  final def filter[IN,OUT,A](tr: Traverser[IN,OUT,A])(f: A => Boolean): Traverser[IN,OUT,A] =
    Traverser(flatMap(tr)(a => if(f(a)) success[OUT,A](a) else fail[OUT,OUT,A]))


  /** Returns the sequential composition of fst and snd which first follows the fst traverser
    * and then feeds the results into the snd traverser.
    * @param fst the first traverser to apply
    * @param snd the subsequent traverser to apply
    * @return the sequential composition of fst and snd
    */
  final def seq[IN1,OUT1,A,OUT2,B](fst: Traverser[IN1,OUT1,A], snd: Traverser[OUT1,OUT2,B]): Traverser[IN1,OUT2,A~B] =
    Traverser(flatMap(fst)(a => map(snd)(b => new ~(a,b))))

  /*
      env => s0 => {
      (map[A,A|B](either) { a => new <|(a) }(env)(s0)) #::: map[B,A|B](or) { a => new |>(a) }(env)(s0)
    }
   */

  /** Returns a traverser which optionally follows the given traverser.
    * @param tr the traverser to follow optionally
    * @return a traverser which optionally follows the given traverser
    */
  final def opt[IN,A](tr: Traverser[IN,IN,A]): Traverser[IN,IN,Option[A]] =
    Traverser(choice(map(success[IN,Unit](()))(_ => None), map(tr)(Some[A](_))))

  /** Returns a traverser which repeats the given traverser 0..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverserN
    */
  final def many[IN,A](tr: Traverser[IN,IN,A]): Traverser[IN,IN,Stream[A]] =
    Traverser(choice(success[IN,Stream[Nothing]](Stream()),many1(tr)))

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
  final def many1[IN,A](tr: Traverser[IN,IN,A]): Traverser[IN,IN,Stream[A]] =
    Traverser(flatMap(tr)(a => map(many(tr))( as => a #:: as)))

}



