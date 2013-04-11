package ch.fhnw.imvs.trails

trait TrailsPrimitives { self: Trails =>
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id

  def V[A](): Traverser[A,Vertex,Vertex]

  def V[A](id: Id): Traverser[A,Vertex,Vertex]

  def V[A](p: Vertex => Boolean): Traverser[A,Vertex,Vertex] =
    for { v <- V[A]() if p(v) } yield v

  def E[A](): Traverser[A,Edge,Edge]

  def E[A](id: Id): Traverser[A,Edge,Edge]

  def E[A](p: Edge => Boolean): Traverser[A,Edge,Edge] =
    for { e <- E[A]() if p(e) } yield e

  def outE(edgeName: String): Traverser[Vertex,Edge,Edge]

  def inE(edgeName: String): Traverser[Vertex,Edge,Edge]

  def outV(): Traverser[Edge,Vertex,Vertex]

  def inV(): Traverser[Edge,Vertex,Vertex]

  def out(edgeName: String): Traverser[Vertex,Vertex,Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex,Vertex,Vertex] =
    inE(edgeName) ~> outV()

  def property[A,T](name: String): Traverser[A,A,T]

  def has[A,T](name: String, value: T): Traverser[A,A,T] =
    property[A,T](name).filter(_ == value)


  final def extendPath[B,E <: PathElement](p: E): Traverser[B,E,Unit] =
    updateState(s => s.copy(path = p :: s.path))

  final def streamToTraverser[B,A](s: Stream[A]): Traverser[B,B,A] = {
    // Requires custom lazyFoldRight because Stream#foldRight is not lazy
    def lazyFoldRight[A, B](xs: Stream[A])(combine: (A, =>B) => B, base: B ): B =
      if (xs.isEmpty) base
      else combine(xs.head,  lazyFoldRight(xs.tail)(combine, base))

    lazyFoldRight(s.map(success))(choice, fail)
  }
}
