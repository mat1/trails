package ch.fhnw.imvs.trails

trait TrailsPrimitives { self: Trails =>
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id

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

  def property[X,A](name: String): Traverser[X,X,A]

  def has[X,A](name: String, value: A): Traverser[X,X,A] =
    property[X,A](name).filter(_ == value)

  final def extendPath[I <: PathElement, O <: PathElement](p: O): Traverser[State[I],State[O],Unit] =
    updateState(s => s.copy(path = p :: s.path))

  final def streamToTraverser[X,A](s: Stream[A]): Traverser[X,X,A] = {
    // Requires custom lazyFoldRight because Stream#foldRight is not lazy
    type TXXA = Traverser[X,X,A]
    def lazyFoldRight(xs: Stream[TXXA])(combine: (TXXA, =>TXXA) => TXXA, base: TXXA): TXXA =
      if (xs.isEmpty) base
      else combine(xs.head, lazyFoldRight(xs.tail)(combine, base))

    lazyFoldRight(s.map(success[X,A]))(choice[X,X,A], fail)
  }
}
