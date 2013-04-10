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

  def V(): Traverser[Vertex] =
    for {
      env <- getEnv
      node <- streamToTraverser(env.getVertices.toStream)
      _ <- extendPath(node)
    } yield node


  def V(id: Id): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- extendPath(node)
    } yield node

  def E(): Traverser[Edge] =
    for {
      env  <- getEnv
      edge <- streamToTraverser(env.getEdges.toStream)
      _    <- extendPath(edge)
    } yield edge


  def E(id: Id): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- extendPath(edge)
    } yield edge

  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    for {
      State((head: Vertex) :: rest) <- getState
      edge <- streamToTraverser(head.getEdges(dir, edgeName).toStream)
      _ <- extendPath(edge)
    } yield edge

  def outV(): Traverser[Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    for {
      s@State((head: Edge) :: rest) <- getState
      vertex = head.getVertex(dir)
      _ <- extendPath(vertex)
    } yield vertex

  def property[T](name: String): Traverser[T] =
    for {
      State(head :: rest) <- getState
    } yield head.getProperty(name).asInstanceOf[T]
}