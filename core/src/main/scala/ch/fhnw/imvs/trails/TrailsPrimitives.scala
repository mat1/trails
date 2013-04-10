package ch.fhnw.imvs.trails

trait TrailsPrimitives { self: Trails =>
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id

  def V(): Traverser[Vertex]

  def V(id: Id): Traverser[Vertex]

  def V(p: Vertex => Boolean): Traverser[Vertex] =
    for { v <- V() if p(v) } yield v

  def E(): Traverser[Edge]

  def E(id: Id): Traverser[Edge]

  def E(p: Edge => Boolean): Traverser[Edge] =
    for { e <- E() if p(e) } yield e

  def outE(edgeName: String): Traverser[Edge]

  def inE(edgeName: String): Traverser[Edge]

  def outV(): Traverser[Vertex]

  def inV(): Traverser[Vertex]

  def out(edgeName: String): Traverser[Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex] =
    inE(edgeName) ~> outV()

  def property[T](name: String): Traverser[T]

  final def extendPath(p: PathElement): Traverser[Unit] =
    updateState(s => s.copy(path = p :: s.path))

  final def streamToTraverser[A](s: Stream[A]): Traverser[A] = {
    // Requires custom lazyFoldRight because Stream#foldRight is not lazy
    def lazyFoldRight[A, B](xs: Stream[A])(combine: (A, =>B) => B, base: B ): B =
      if (xs.isEmpty) base
      else combine(xs.head,  lazyFoldRight(xs.tail)(combine, base))

    lazyFoldRight(s.map(success))(choice, fail)
  }
}
