package ch.fhnw.imvs.trails.neo4j


import org.neo4j.graphdb.Direction._
import org.neo4j.tooling.GlobalGraphOperations
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import scala.collection.JavaConversions._
import org.neo4j.graphdb._
import reflect.ClassTag


object Neo4jTrails extends TrailsPrimitives with Trails {
  type Environment = GraphDatabaseService
  type PathElement = PropertyContainer
  type Edge = Relationship
  type Vertex = Node
  type Id = Long

  def V(): Traverser[Vertex] =
    env => path => GlobalGraphOperations.at(env).getAllNodes().toStream.map { node => (node :: path, node) }

  def V(id: Id): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getNodeById(id)
      _    <- updatePath(p => node :: p)
    } yield node

  def E(): Traverser[Edge] =
    env => path => GlobalGraphOperations.at(env).getAllRelationships().toStream.map { edge => (edge :: path, edge) }

  def E(id: Id): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getRelationshipById(id)
      _    <- updatePath(p => edge :: p)
    } yield edge

  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUTGOING)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, INCOMING)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    env => path => path match {
      case (head: Vertex) :: rest =>
        val rels = head.getRelationships(DynamicRelationshipType.withName(edgeName), dir)
        rels.toStream.map { edge => ((edge :: path), edge) }
    }

  def outV(): Traverser[Vertex] =
    ontoV(OUTGOING)

  def inV(): Traverser[Vertex] =
    ontoV(INCOMING)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    for {
      path@((head: Edge) :: rest) <- getPath
      v = if(dir == OUTGOING) head.getStartNode() else head.getEndNode()
      _ <- setPath(v :: path)
    } yield v

  def property[T:ClassTag](name: String): Traverser[T] = {
    for {
      head :: rest <- getPath
    } yield head.getProperty(name).asInstanceOf[T]
  }
}
/*
trait Neo4jTrails extends TrailsPrimitives with Trails {
  type Environment = GraphDatabaseService
  type PathElement =
  type Node =

  // Gremlin like operations
  def outE(edgeName: String): Traverser =
    ontoE(edgeName, OUTGOING)

  def inE(edgeName: String): Traverser =
    ontoE(edgeName, INCOMING)

  private def ontoE(edgeName: String, dir: Direction): Traverser =
    e => ts => ts match {
      case (t@Trace(((head: Node) :: rest), visitedPaths), state) =>
        head.getRelationships(DynamicRelationshipType.withName(edgeName), dir).toStream.map { edge => (Trace((edge :: t.path), visitedPaths), state) }
    }

  def outV(): Traverser =
    ontoV(OUTGOING)

  def inV(): Traverser =
    ontoV(INCOMING)

  private def ontoV(dir: Direction): Traverser =
    _ => ts => ts match {
      case (t@Trace(((head: Relationship) :: rest), visitedPaths), state) =>
        Stream((Trace((if(dir == OUTGOING) head.getStartNode() else head.getEndNode())  :: t.path, visitedPaths), state))
    }

  def out(edgeName: String): Traverser =
    product(outE(edgeName), inV())

  def in(edgeName: String): Traverser =
    product(inE(edgeName), outV())



  def V(id: Long): Traverser =
    env => ts => Stream((Trace(List(env.getNodeById(id)), None), ts._2))

  def E(): Traverser =
    env => ts => GlobalGraphOperations.at(env).getAllRelationships().toStream.map(v => (Trace(List(v), None), ts._2))

  def E(id: Long): Traverser =
    env => ts => Stream((Trace(List(env.getRelationshipById(id)), None), ts._2))

  /*
  def has(name: String, value: Any): Traverser =
    filterHead(elem => elem.hasProperty(name) && elem.getProperty(name) == value)

  def matches(pattern: Traverser): Traverser =
    env => filter(t => pattern(env)(t).nonEmpty)(env)
    */

}
*/

