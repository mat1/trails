package ch.fhnw.imvs.trails.neo4j

import org.neo4j.graphdb._
import org.neo4j.graphdb.Direction._
import org.neo4j.tooling.GlobalGraphOperations
import ch.fhnw.imvs.trails.Trails
import scala.collection.JavaConversions._

//TODO DK: If we decide to use the same method names (outE, inV, etc.) for all underlying graphs, then we should factor it out into a trait.
trait Neo4jTrails extends Trails {
  type Environment = GraphDatabaseService
  type PathElement = PropertyContainer
  type State = Unit

  // Gremlin like operations
  def outE(edgeName: String): Traverser =
    ontoE(edgeName, OUTGOING)

  def inE(edgeName: String): Traverser =
    ontoE(edgeName, INCOMING)

  private def ontoE(edgeName: String, dir: Direction): Traverser =
    e => ts => ts match {
      case (t@Trace(((head: Node) :: rest), namedSubpaths, visitedPaths), state) =>
        head.getRelationships(DynamicRelationshipType.withName(edgeName), dir).toStream.map { edge => (Trace((edge :: t.path), namedSubpaths, visitedPaths), state) }
    }

  def outV(): Traverser =
    ontoV(OUTGOING)

  def inV(): Traverser =
    ontoV(INCOMING)

  private def ontoV(dir: Direction): Traverser =
    _ => ts => ts match {
      case (t@Trace(((head: Relationship) :: rest), namedSubpaths, visitedPaths), state) =>
        Stream((Trace((if(dir == OUTGOING) head.getStartNode() else head.getEndNode())  :: t.path, namedSubpaths, visitedPaths), state))
    }

  def out(edgeName: String): Traverser =
    seq(outE(edgeName), inV())

  def in(edgeName: String): Traverser =
    seq(inE(edgeName), outV())

  def V(): Traverser =
    env => ts => GlobalGraphOperations.at(env).getAllNodes().toStream.map(v => (Trace(List(v), Map(), None), ts._2))

  def V(id: Long): Traverser =
    env => ts => Stream((Trace(List(env.getNodeById(id)), Map(), None), ts._2))

  def E(): Traverser =
    env => ts => GlobalGraphOperations.at(env).getAllRelationships().toStream.map(v => (Trace(List(v), Map(), None), ts._2))

  def E(id: Long): Traverser =
    env => ts => Stream((Trace(List(env.getRelationshipById(id)), Map(), None), ts._2))

  def has(name: String, value: Any): Traverser =
    filterHead(elem => elem.hasProperty(name) && elem.getProperty(name) == value)

  def matches(pattern: Traverser): Traverser =
    env => filter(t => pattern(env)(t).nonEmpty)(env)

  def format(e: PathElement): String = e match {
    case v:Node => v.getId().toString
    case e:Relationship => "-" + e.getId().toString + "-"
  }

  def show(t: Trace): String =
    "Trace" + t.path.reverse.map(format).mkString("["," ","]")
}
