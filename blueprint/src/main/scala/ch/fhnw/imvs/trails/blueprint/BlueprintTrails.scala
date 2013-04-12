package ch.fhnw.imvs.trails.blueprint

import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import com.tinkerpop.blueprints.Direction
import com.tinkerpop.blueprints.Direction._

object BlueprintTrails extends TrailsPrimitives with Trails {
  type Environment = com.tinkerpop.blueprints.Graph
  type PathElement = com.tinkerpop.blueprints.Element
  type Edge = com.tinkerpop.blueprints.Edge
  type Vertex = com.tinkerpop.blueprints.Vertex
  type Id = Any

  def V(): Traverser[State[Nothing],State[Vertex],Vertex] =
    for {
      env <- getEnv
      node <- streamToTraverser[State[Nothing],Vertex](env.getVertices.toStream)
      _ <- extendPath[Nothing,Vertex](node)
    } yield node

  def V(id: Id): Traverser[State[Nothing],State[Vertex],Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- extendPath[Nothing,Vertex](node)
    } yield node

  def E(): Traverser[State[Nothing],State[Edge],Edge] =
    for {
      env  <- getEnv
      edge <- streamToTraverser[State[Nothing],Edge](env.getEdges.toStream)
      _    <- extendPath[Nothing,Edge](edge)
    } yield edge


  def E(id: Id): Traverser[State[Nothing],State[Edge],Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- extendPath[Nothing,Edge](edge)
    } yield edge

  def outE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[State[Vertex],State[Edge],Edge] =
    for {
      State((head: Vertex) :: rest) <- getState[State[Vertex]]
      edge <- streamToTraverser[State[Vertex],Edge](head.getEdges(dir, edgeName).toStream)
      _ <- extendPath[Vertex,Edge](edge)
    } yield edge

  def outV(): Traverser[State[Edge],State[Vertex],Vertex] =
    ontoV(OUT)

  def inV(): Traverser[State[Edge],State[Vertex],Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[State[Edge],State[Vertex],Vertex] =
    for {
      State((head: Edge) :: rest) <- getState[State[Edge]]
      vertex = head.getVertex(dir)
      _ <- extendPath[Edge,Vertex](vertex)
    } yield vertex

  def property[X,A](name: String): Traverser[X,X,A] =
    for {
      State(head :: rest) <- getState[X]
    } yield head.getProperty(name).asInstanceOf[A]
}