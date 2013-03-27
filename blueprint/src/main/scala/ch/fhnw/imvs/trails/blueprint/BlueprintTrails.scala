package ch.fhnw.imvs.trails.blueprint


import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import com.tinkerpop.blueprints.Direction
import com.tinkerpop.blueprints.Direction._
import reflect.ClassTag


trait BlueprintTrails extends TrailsPrimitives with Trails {
  type Environment = com.tinkerpop.blueprints.Graph
  type PathElement = com.tinkerpop.blueprints.Element
  type Edge = com.tinkerpop.blueprints.Edge
  type Vertex = com.tinkerpop.blueprints.Vertex

  def V(): Traverser[Vertex] =
    env => path => env.getVertices.toStream.map { node => (node :: path, node) }

  def V(id: AnyRef): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- updatePath(p => node :: p) //TODO extend path or drop it on V()
    } yield node

  def E(): Traverser[Edge] =
    env => path => env.getEdges.toStream.map { edge => (edge :: path, edge) }

  def E(id: AnyRef): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- updatePath(p => edge :: p)
    } yield edge

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
      path@((head: Edge) :: rest) <- getPath
      v = head.getVertex(dir)
      _ <- setPath(v :: path)
    } yield v

  def property[T:ClassTag](name: String): Traverser[T] = {
    for {
      head :: rest <- getPath
    } yield head.getProperty(name).asInstanceOf[T]
  }
}