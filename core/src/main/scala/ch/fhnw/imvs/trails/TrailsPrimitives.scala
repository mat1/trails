package ch.fhnw.imvs.trails

trait TrailsPrimitives { self: Trails =>
  /** Type of a single element in the graph. Typically a common super type of Vertex and Edge. */
  type PathElement
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id
  type Environment
  type Traverser[I,O,A] = Tr[Environment,I,O,A]

  type Path = List[PathElement]
  final case class State[+Head <: PathElement](path: Path)
  // final case class State[+Head <: PathElement](head: Head, tail: List[PathElement])

  object Traverser {
    def run[P <: PathElement,A](tr: Traverser[State[Nothing],State[P],A], env: Environment): Stream[(Path,A)] =
      tr(env)(State[Nothing](Nil)).map { case (s,a) => (s.path.reverse, a) }
  }

  def V(): Traverser[State[Nothing],State[Vertex],Vertex]

  def V(id: Id): Traverser[State[Nothing],State[Vertex],Vertex]

  def V(p: Vertex => Boolean): Traverser[State[Nothing],State[Vertex],Vertex] =
    for { v <- V() if p(v) } yield v

  def E(): Traverser[State[Nothing],State[Edge],Edge]

  def E(id: Id): Traverser[State[Nothing],State[Edge],Edge]

  def E(p: Edge => Boolean): Traverser[State[Nothing],State[Edge],Edge] =
    for { e <- E() if p(e) } yield e

  def outE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge]

  def inE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge]

  def outV(): Traverser[State[Edge],State[Vertex],Vertex]

  def inV(): Traverser[State[Edge],State[Vertex],Vertex]

  def out(edgeName: String): Traverser[State[Vertex],State[Vertex],Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[State[Vertex],State[Vertex],Vertex] =
    inE(edgeName) ~> outV()

  final def extendPath[I <: PathElement, O <: PathElement](p: O): Traverser[State[I],State[O],Unit] =
    updateState(s => s.copy(path = p :: s.path))

  final def streamToTraverser[S,A](s: Stream[A]): Traverser[S,S,A] = {
    // Requires custom lazyFoldRight because Stream#foldRight is not lazy
    type TSSA = Traverser[S,S,A]
    def lazyFoldRight(xs: Stream[TSSA])(combine: (TSSA, =>TSSA) => TSSA, base: TSSA): TSSA =
      if (xs.isEmpty) base
      else combine(xs.head, lazyFoldRight(xs.tail)(combine, base))

    lazyFoldRight(s.map(success[Environment,S,A]))(choice[Environment,S,S,A], fail)
  }

  def get[A](name: String)(e: PathElement): A
}
