package ch.fhnw.imvs.trails

import reflect.ClassTag


trait TrailsPrimitives { self: Trails =>
  type Edge <: PathElement
  type Vertex <: PathElement
  type Id

  def V(): Traverser[Vertex]

  def V(id: Id): Traverser[Vertex]

  def E(): Traverser[Edge]

  def E(id: Id): Traverser[Edge]

  def outE(edgeName: String): Traverser[Edge]

  def inE(edgeName: String): Traverser[Edge]

  def outV(): Traverser[Vertex]

  def inV(): Traverser[Vertex]

  def out(edgeName: String): Traverser[Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex] =
    inE(edgeName) ~> outV()

  def property[T:ClassTag](name: String): Traverser[T]
}
