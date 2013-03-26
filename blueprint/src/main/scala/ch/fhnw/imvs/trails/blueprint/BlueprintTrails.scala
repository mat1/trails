package ch.fhnw.imvs.trails.blueprint

import com.tinkerpop.blueprints.Direction._
import com.tinkerpop.blueprints._
import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.{Trails}
import scalaz.Show
import reflect.ClassTag

trait BlueprintTrails extends Trails{
  type Environment = Graph
  type PathElement = Element

  // Gremlin like operations
  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    e => path => path match {
      case (head: Vertex) :: rest =>
        head.getEdges(dir, edgeName).toStream.map { edge => ((edge :: path), edge) }
    }

  def outV(): Traverser[Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    for {
      path@((head: Edge) :: rest) <- getState
      v = head.getVertex(dir)
      _ <- setState(v :: path)
    } yield v


  def out(edgeName: String): Traverser[Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex] =
    inE(edgeName) ~> outV()

  def V(): Traverser[Vertex] =
    env => path => env.getVertices.toStream.map { node => (node :: path, node) }

  def V(id: AnyRef): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- updateState(p => node :: p) //TODO extend path or drop it on V()
    } yield node

  def E(): Traverser[Edge] =
    env => path => env.getEdges.toStream.map { edge => (edge :: path, edge) }

  def E(id: AnyRef): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- updateState(p => edge :: p)
    } yield edge
}