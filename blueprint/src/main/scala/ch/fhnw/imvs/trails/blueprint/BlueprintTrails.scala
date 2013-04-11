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

  def V[A](): Traverser[A,Vertex,Vertex] =
    Traverser(for {
      env <- getEnv
      node <- streamToTraverser[A,Vertex](env.getVertices.toStream)
      _ <- extendPath[A,Vertex](node)
    } yield node)


  def V[A](id: Id): Traverser[A,Vertex,Vertex] =
    Traverser(for {
      env  <- getEnv[A]
      node = env.getVertex(id)
      _    <- extendPath[A,Vertex](node)
    } yield node)

  def E[A](): Traverser[A,Edge,Edge] =
    Traverser(for {
      env  <- getEnv
      edge <- streamToTraverser[A,Edge](env.getEdges.toStream)
      _    <- extendPath[A,Edge](edge)
    } yield edge)


  def E[A](id: Id): Traverser[A,Edge,Edge] =
    Traverser(for {
      env  <- getEnv[A]
      edge = env.getEdge(id)
      _    <- extendPath[A,Edge](edge)
    } yield edge)

  def outE(edgeName: String): Traverser[Vertex,Edge,Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Vertex,Edge,Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Vertex,Edge,Edge] =
    Traverser(for {
      State((head: Vertex) :: rest, _) <- getState[Vertex]
      edge <- streamToTraverser[Vertex,Edge](head.getEdges(dir, edgeName).toStream)
      _ <- extendPath[Vertex,Edge](edge)
    } yield edge)

  def outV(): Traverser[Edge,Vertex,Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Edge,Vertex,Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Edge,Vertex,Vertex] =
    Traverser[Edge,Vertex,Vertex](for {
      s@State((head: Edge) :: rest, _) <- getState[Edge]
      vertex = head.getVertex(dir)
      _ <- extendPath[Edge,Vertex](vertex)
    } yield vertex)

  def property[A,T](name: String): Traverser[A,A,T] =
    Traverser(for {
      State(head :: rest, _) <- getState
    } yield head.getProperty(name).asInstanceOf[T])
}