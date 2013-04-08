package ch.fhnw.imvs.trails.blueprint


import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import com.tinkerpop.blueprints.Direction
import com.tinkerpop.blueprints.Direction._
import reflect.ClassTag


object BlueprintTrails extends TrailsPrimitives with Trails {
  type Environment = com.tinkerpop.blueprints.Graph
  type PathElement = com.tinkerpop.blueprints.Element
  type Edge = com.tinkerpop.blueprints.Edge
  type Vertex = com.tinkerpop.blueprints.Vertex
  type Id = Any

  def V(): Traverser[Vertex] =
    env => s => env.getVertices.toStream.map { node => (s.copy(path = node :: s.path), node) }

  def V(id: Id): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- updateState(s => s.copy(path = node :: s.path))
    } yield node

  def E(): Traverser[Edge] =
    env => s => env.getEdges.toStream.map { edge => (s.copy(path = edge :: s.path), edge) }

  def E(id: Id): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- updateState(s => s.copy(path = edge :: s.path))
    } yield edge

  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    env => s => s match {
      case State((head: Vertex) :: rest) =>
        head.getEdges(dir, edgeName).toStream.map { edge => (s.copy(path = (edge :: s.path)), edge) }
    }

  def outV(): Traverser[Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    for {
      s@State((head: Edge) :: rest) <- getState
      v = head.getVertex(dir)
      _ <- setState(s.copy(path = v :: s.path))
    } yield v

  def property[T:ClassTag](name: String): Traverser[T] = {
    for {
      State(head :: rest) <- getState
    } yield head.getProperty(name).asInstanceOf[T]
  }
}