package ch.fhnw.imvs.trails.blueprint

import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import com.tinkerpop.blueprints.{Graph, Direction}
import com.tinkerpop.blueprints.Direction._

object BlueprintTrails extends TrailsPrimitives with Trails {
  type Environment = Graph
  type PathElement = com.tinkerpop.blueprints.Element
  type Edge = com.tinkerpop.blueprints.Edge
  type Vertex = com.tinkerpop.blueprints.Vertex
  type Id = Any


  def V(): Traverser[State[Nothing],State[Vertex],Vertex] =
    for {
      env <- getEnv[Environment,State[Nothing]]
      v   <- streamToTraverser[State[Nothing],Vertex](env.getVertices.toStream)
      _   <- extendPath[Nothing,Vertex](v)
    } yield v

  def V(id: Id): Traverser[State[Nothing],State[Vertex],Vertex] =
    for {
      env <- getEnv[Environment,State[Nothing]]
      v = env.getVertex(id)
      _   <- extendPath[Nothing,Vertex](v)
    } yield v

  def E(): Traverser[State[Nothing],State[Edge],Edge] =
    for {
      env <- getEnv[Environment,State[Nothing]]
      e   <- streamToTraverser[State[Nothing],Edge](env.getEdges.toStream)
      _   <- extendPath[Nothing,Edge](e)
    } yield e


  def E(id: Id): Traverser[State[Nothing],State[Edge],Edge] =
    for {
      env <- getEnv[Environment,State[Nothing]]
      e   = env.getEdge(id)
      _   <- extendPath[Nothing,Edge](e)
    } yield e

  def outE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[State[Vertex],State[Edge],Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[State[Vertex],State[Edge],Edge] =
    for {
      State((head: Vertex) :: rest) <- getState[Graph,State[Vertex]]
      e <- streamToTraverser[State[Vertex],Edge](head.getEdges(dir, edgeName).toStream)
      _ <- extendPath[Vertex,Edge](e)
    } yield e

  def outV(): Traverser[State[Edge],State[Vertex],Vertex] =
    ontoV(OUT)

  def inV(): Traverser[State[Edge],State[Vertex],Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[State[Edge],State[Vertex],Vertex] =
    for {
      State((head: Edge) :: rest) <- getState[Graph,State[Edge]]
      v = head.getVertex(dir)
      _ <- extendPath[Edge,Vertex](v)
    } yield v

  def property[S,A](name: String): Traverser[S,S,A] =
    for {
      State(head :: rest) <- getState[Graph,S]
    } yield head.getProperty(name).asInstanceOf[A]
}