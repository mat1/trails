package ch.fhnw.imvs.trails.blueprint

import com.tinkerpop.blueprints.Direction._
import com.tinkerpop.blueprints._
import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.Trails
import scalaz.Show

trait BlueprintTrails extends Trails {

  type Environment = Graph
  type PathElement = Element
  type State = Unit


  implicit val showElement: Show[PathElement] = new Show[PathElement] {
    override def shows(e: PathElement) = e match {
      case v:Vertex => v.getId.toString
      case e:Edge => "-" + e.getId.toString + "-"
    }
  }

  // Gremlin like operations
  def outE(edgeName: String): Traverser =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser =
    e => ts => ts match {
      case (t@Trace(((head: Vertex) :: rest), namedSubpaths, visitedPaths), state) =>
        head.getEdges(dir, edgeName).toStream.map { edge => (Trace((edge :: t.path), namedSubpaths, visitedPaths), state) }
    }

  def outV(): Traverser =
    ontoV(OUT)

  def inV(): Traverser =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser =
    _ => ts => ts match {
      case (t@Trace(((head: Edge) :: rest), namedSubpaths, visitedPaths), state) =>
        Stream((Trace((head.getVertex(dir) :: t.path), namedSubpaths, visitedPaths),state))
    }

  def out(edgeName: String): Traverser =
    seq(outE(edgeName), inV())

  def in(edgeName: String): Traverser =
    seq(inE(edgeName), outV())

  def V(): Traverser =
    env => ts => env.getVertices.toStream.map(v => (Trace(List(v), Map(), None), ts._2))

  def V(id: AnyRef): Traverser =
    env => ts => Stream((Trace(List(env.getVertex(id)), Map(), None), ts._2))

  def E(): Traverser =
    env => ts => env.getEdges.toStream.map(v => (Trace(List(v), Map(), None), ts._2))

  def E(id: AnyRef): Traverser =
    env => ts => Stream((Trace(List(env.getEdge(id)), Map(), None), ts._2))

  def has(name: String, value: Any): Traverser =
    filterHead(elem => elem.getPropertyKeys.contains(name) && elem.getProperty(name) == value)

  def matches(pattern: Traverser): Traverser =
    env => filter(t => pattern(env)(t).nonEmpty)(env)
}