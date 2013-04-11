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

  def V[X](): Traverser[X,Vertex,Vertex] =
    Traverser(for {
      env <- getEnv
      node <- streamToTraverser[X,Vertex](env.getVertices.toStream)
      _ <- extendPath[X,Vertex](node)
    } yield node)

  def V[X](id: Id): Traverser[X,Vertex,Vertex] =
    Traverser(for {
      env  <- getEnv[X]
      node = env.getVertex(id)
      _    <- extendPath[X,Vertex](node)
    } yield node)

  def E[X](): Traverser[X,Edge,Edge] =
    Traverser(for {
      env  <- getEnv
      edge <- streamToTraverser[X,Edge](env.getEdges.toStream)
      _    <- extendPath[X,Edge](edge)
    } yield edge)


  def E[X](id: Id): Traverser[X,Edge,Edge] =
    Traverser(for {
      env  <- getEnv[X]
      edge = env.getEdge(id)
      _    <- extendPath[X,Edge](edge)
    } yield edge)

  def outE(edgeName: String): Traverser[Vertex,Edge,Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Vertex,Edge,Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Vertex,Edge,Edge] =
    Traverser(for {
      State((head: Vertex) :: rest) <- getState[Vertex]
      edge <- streamToTraverser[Vertex,Edge](head.getEdges(dir, edgeName).toStream)
      _ <- extendPath[Vertex,Edge](edge)
    } yield edge)

  def outV(): Traverser[Edge,Vertex,Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Edge,Vertex,Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Edge,Vertex,Vertex] =
    Traverser[Edge,Vertex,Vertex](for {
      State((head: Edge) :: rest) <- getState[Edge]
      vertex = head.getVertex(dir)
      _ <- extendPath[Edge,Vertex](vertex)
    } yield vertex)

  def property[X,A](name: String): Traverser[X,X,A] =
    Traverser(for {
      State(head :: rest) <- getState
    } yield head.getProperty(name).asInstanceOf[A])
}