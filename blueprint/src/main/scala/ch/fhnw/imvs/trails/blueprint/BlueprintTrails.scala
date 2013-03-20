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
      case (path@((head: Vertex) :: rest), visitedPaths, l) =>
        head.getEdges(dir, edgeName).toStream.map { edge => (((edge :: path), visitedPaths, l), edge) }
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


  def out(edgeName: String): Traverser[Vertex] =
    outE(edgeName) ~> inV()

  def in(edgeName: String): Traverser[Vertex] =
    inE(edgeName) ~> outV()

  def V(): Traverser[Vertex] =
    env => ts => env.getVertices.toStream.map { node =>
      ((List(node), None, Map[String,List[Path]]()),node)
    }

  def V(id: AnyRef): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getVertex(id)
      _    <- updatePath(p => node :: p)
    } yield node


  def E(): Traverser[Edge] =
    env => ts => env.getEdges.toStream.map { edge =>
      ((List(edge), None, Map[String,List[Path]]()), edge)
    }

  def E(id: AnyRef): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getEdge(id)
      _    <- updatePath(p => edge :: p)
    } yield edge
}