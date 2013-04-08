package ch.fhnw.imvs.trails

import scala.language.implicitConversions

/** trails provides purely functional graph traverser combinators. */
trait Trails { self =>

  /** Type of the environment available to every Traverser. */
  type Environment

  /** Type of a single element in the graph. Typically a common super type of Vertex and Edge. */
  type PathElement

  type Path = List[PathElement]

  type Traverser[+A] = Environment => Path => Stream[(Path,A)]

  object Traverser {
    def run[A](tr: Traverser[A], env: Environment): Stream[(Path,A)] =
      tr(env)(Nil).map { case (path,a) => (path.reverse,a) }
  }

  /** Provides some nice infix syntax. */
  final implicit class Syntax[A](t1: Traverser[A]) {
    def ~[B](t2: Traverser[B]): Traverser[A~B] = sequence(t1, t2)
    def ~>[B](t2: Traverser[B]): Traverser[B] = self.map(sequence(t1, t2)){ case a ~ b => b }
    def <~[B](t2: Traverser[B]): Traverser[A] = self.map(sequence(t1, t2)){ case a ~ b => a }
    def |(t2: Traverser[A]): Traverser[A] = choice(t1, t2)
    def ? : Traverser[Option[A]] = optional(t1)
    def * : Traverser[Stream[A]] = many(t1)
    def + : Traverser[Stream[A]] = many1(t1)

    def ^^[B](f: A => B): Traverser[B] = map(f)

    /* for-comprehension sugar */
    def flatMap[B](f: A => Traverser[B]): Traverser[B] = self.flatMap(t1)(f)
    def map[B](f: A => B): Traverser[B] = self.map(t1)(f)
    def filter(p: A => Boolean): Traverser[A] = self.filter(t1)(p)

    def withFilter(p: A => Boolean): TraverserWithFilter[A] = new TraverserWithFilter(t1, p)
    final class TraverserWithFilter[+A](self: Traverser[A], p: A => Boolean) {
      def map[B](f: A => B): Traverser[B] = self filter p map f
      def flatMap[B](f: A => Traverser[B]): Traverser[B] = self filter p flatMap f
      def withFilter(q: A => Boolean): TraverserWithFilter[A] = new TraverserWithFilter[A](self, x => p(x) && q(x))
    }
  }

  final case class ~[+A,+B](a: A, b: B) // Product
  sealed trait <|>[+A,+B] // Sum
  final case class <|[A](a: A) extends <|>[A,Nothing]
  final case class |>[B](a: B) extends <|>[Nothing,B]

  final def flatMap[A,B](t: Traverser[A])(f: A => Traverser[B]): Traverser[B] =
    env => path0 => t(env)(path0).flatMap { case (path1,a) => f(a)(env)(path1) }
    //env => path0 => for { (path1,a) <- t(env)(path0); (path2,b) <- f(a)(env)(path1) } yield (path2, b)

  final def map[A,B](tr: Traverser[A])(f: A => B): Traverser[B] =
    flatMap(tr)(a => success(f(a)))

  final def filter[A](tr: Traverser[A])(f: A => Boolean): Traverser[A] =
    flatMap(tr)(a => if(f(a)) success(a) else fail)

  final def getEnv: Traverser[Environment] =
    env => path => Stream((path, env))

  final def getPath: Traverser[Path] =
    env => path => Stream((path, path))

  final def setPath(state: Path): Traverser[Unit] =
    env => _ => Stream((state, ()))

  final def updatePath(f: Path => Path): Traverser[Unit] =
    for {
      s <- getPath
      _ <- setPath(f(s))
    } yield ()

  /** Returns the sequential composition of fst and snd which first follows the fst traverser
    * and then feeds the results into the snd traverser.
    * @param fst the first traverser to apply
    * @param snd the subsequent traverser to apply
    * @return the sequential composition of fst and snd
    */
  final def sequence[A,B](fst: Traverser[A], snd: Traverser[B]): Traverser[A~B] =
    flatMap(fst)(a => map(snd)(b => new ~(a,b)))

  /** Returns the 'parallel' composition of the two given traversers which follows both alternatives.
    * @param either one of the traverser to follow
    * @param or one of the traverser to follow
    * @return the parallel composition of the two given traversers
    */
  final def choice[A](either: Traverser[A], or: => /* this is important */ Traverser[A]): Traverser[A] =
    env => path => either(env)(path) #::: or(env)(path)
  /*
      env => s0 => {
      (map[A,A|B](either) { a => new <|(a) }(env)(s0)) #::: map[B,A|B](or) { a => new |>(a) }(env)(s0)
    }
   */

  /** Returns a traverser which optionally follows the given traverser.
    * @param tr the traverser to follow optionally
    * @return a traverser which optionally follows the given traverser
    */
  final def optional[A](tr: Traverser[A]): Traverser[Option[A]] =
    choice(map(success(()))(_ => None), map(tr)(Some[A](_)))

  /** Returns a traverser which repeats the given traverser 0..* times.
    * @param tr the traverser to be repeated
    * @return a traverser which repeats the given traverserN
    */
  final def many[A](tr: Traverser[A]): Traverser[Stream[A]] =
    choice(success(Stream()), many1(tr))

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
  final def many1[A](tr: Traverser[A]): Traverser[Stream[A]] =
    for { a <- tr; as <- many(tr) } yield { a #:: as }




  /** Returns a traverser which returns its input as the output.
    * @return a traverser which returns its input
    */
  final def success[A](a: A): Traverser[A] =
    _ => s0 => Stream((s0,a))

  /** Returns a traverser which drops its input and returns a empty output.
    * @return a traverser which drops its input
    */
  final def fail[A]: Traverser[A] =
    _ => _ => Stream()
}


