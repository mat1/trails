package ch.fhnw.imvs.trails.blueprint

import com.tinkerpop.blueprints.Direction._
import com.tinkerpop.blueprints._
import scala.collection.JavaConversions._
import ch.fhnw.imvs.trails.Trails

trait BlueprintTrails extends Trails {

  type Environment = Graph
  type PathElement = Element

  // Gremlin like operations
  def outE(edgeName: String): Traverser =
    ontoE(edgeName, OUT)

  def inE(edgeName: String): Traverser =
    ontoE(edgeName, IN)

  private def ontoE(edgeName: String, dir: Direction): Traverser =
    e => t => t match {
      case Trace(((head: Vertex) :: rest), visitedPaths) =>
        head.getEdges(dir, edgeName).toStream.map { edge => Trace((edge :: t.path), visitedPaths) }
    }

  def outV(): Traverser =
    ontoV(OUT)

  def inV(): Traverser =
    ontoV(IN)

  private def ontoV(dir: Direction): Traverser =
    _ => t => t match {
      case Trace(((head: Edge) :: rest), visitedPaths) =>
        Stream(Trace((head.getVertex(dir) :: t.path), visitedPaths))
    }

  def out(edgeName: String): Traverser =
    seq(outE(edgeName), inV())

  def in(edgeName: String): Traverser =
    seq(inE(edgeName), outV())

  def V(): Traverser =
    env => _ => env.getVertices.toStream.map(v => Trace(List(v), None))

  def V(id: AnyRef): Traverser =
    env => _ => Stream(Trace(List(env.getVertex(id)), None))

  def E(): Traverser =
    env => _ => env.getEdges.toStream.map(v => Trace(List(v), None))

  def E(id: AnyRef): Traverser =
    env => _ => Stream(Trace(List(env.getEdge(id)), None))

  def has(name: String, value: Any): Traverser =
    filterHead(elem => elem.getPropertyKeys.contains(name) && elem.getProperty(name) == value)

  def matches(pattern: Traverser): Traverser =
    env => filter(t => pattern(env)(t).nonEmpty)(env)

  def format(e: PathElement): String = e match {
    case v:Vertex => v.getId.toString
    case e:Edge => "-" + e.getId.toString + "-"
  }

  def show(t: Trace): String =
    "Trace" + t.path.reverse.map(format).mkString("["," ","]")
}