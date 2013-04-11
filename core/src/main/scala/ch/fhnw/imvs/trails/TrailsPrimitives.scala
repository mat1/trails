package ch.fhnw.imvs.trails

trait TrailsPrimitives { self: Trails =>
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id

  def V[X](): Traverser[X,Vertex,Vertex]

  def V[X](id: Id): Traverser[X,Vertex,Vertex]

  def V[X](p: Vertex => Boolean): Traverser[X,Vertex,Vertex] =
    for { v <- V[X]() if p(v) } yield v

  def E[X](): Traverser[X,Edge,Edge]

  def E[X](id: Id): Traverser[X,Edge,Edge]

  def E[X](p: Edge => Boolean): Traverser[X,Edge,Edge] =
    for { e <- E[X]() if p(e) } yield e

  def outE(edgeName: String): Traverser[Vertex,Edge,Edge]

  def inE(edgeName: String): Traverser[Vertex,Edge,Edge]

  def outV(): Traverser[Edge,Vertex,Vertex]

  def inV(): Traverser[Edge,Vertex,Vertex]

  def out(edgeName: String): Traverser[Vertex,Vertex,Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex,Vertex,Vertex] =
    inE(edgeName) ~> outV()

  def property[X,A](name: String): Traverser[X,X,A]

  def has[X,A](name: String, value: A): Traverser[X,X,A] =
    property[X,A](name).filter(_ == value)

  final def extendPath[X,E <: PathElement](p: E): Traverser[X,E,Unit] =
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
