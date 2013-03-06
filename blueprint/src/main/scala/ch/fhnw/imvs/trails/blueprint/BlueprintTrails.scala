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
  type AdditionalState = Unit

  implicit val showElement: Show[PathElement] = new Show[PathElement] {
    override def shows(e: PathElement) = e match {
      case v:Vertex => v.getId.toString
      case e:Edge => "-" + e.getId.toString + "-"
    }
  }

  // Gremlin like operations
  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    e => ts => ts match {
      case (path@((head: Vertex) :: rest), visitedPaths) =>
        head.getEdges(dir, edgeName).toStream.map { edge => (((edge :: path), visitedPaths), edge) }
    }

  def outV(): Traverser[Vertex] =
    ontoV(OUT)

  def inV(): Traverser[Vertex] =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    _ => ts => ts match {
      case (path@((head: Edge) :: rest), visitedPaths) => {
        val v = head.getVertex(dir)
        Stream((((v :: path), visitedPaths), v))
      }
    }

  def out(edgeName: String): Traverser[Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex] =
    inE(edgeName) ~> outV()

  def V(): Traverser[Vertex] =
    env => ts => env.getVertices.toStream.map { node =>
      ((List(node), None),node)
    }

  def V(id: AnyRef): Traverser[Vertex] =
    env => ts => {
      val node = env.getVertex(id)
      Stream(((List(node), None),node))
    }

  def E(): Traverser[Edge] =
    env => ts => env.getEdges.toStream.map { edge =>
      ((List(edge), None),edge)
    }

  def E(id: AnyRef): Traverser[Edge] =
    env => ts => {
      val edge = env.getEdge(id)
      Stream(((List(edge), None),edge))
    }
}