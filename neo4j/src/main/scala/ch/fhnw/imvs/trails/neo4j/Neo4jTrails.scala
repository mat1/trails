package ch.fhnw.imvs.trails.neo4j
/*
import org.neo4j.graphdb.Direction._
import org.neo4j.tooling.GlobalGraphOperations
import ch.fhnw.imvs.trails.{TrailsPrimitives, Trails}
import scala.collection.JavaConversions._
import org.neo4j.graphdb._

object Neo4jTrails extends TrailsPrimitives with Trails {
  type Environment = GraphDatabaseService
  type PathElement = PropertyContainer
  type Edge = Relationship
  type Vertex = Node
  type Id = Long

  def V(): Traverser[Vertex] =
    for {
      env <- getEnv
      allNodes = GlobalGraphOperations.at(env).getAllNodes()
      node <- streamToTraverser(allNodes.toStream)
      _ <- extendPath(node)
    } yield node

  def V(id: Id): Traverser[Vertex] =
    for {
      env  <- getEnv
      node = env.getNodeById(id)
      _    <- extendPath(node)
    } yield node

  def E(): Traverser[Edge] =
    for {
      env <- getEnv
      allEdges = GlobalGraphOperations.at(env).getAllRelationships()
      edge <- streamToTraverser(allEdges.toStream)
      _ <- extendPath(edge)
    } yield edge

  def E(id: Id): Traverser[Edge] =
    for {
      env  <- getEnv
      edge = env.getRelationshipById(id)
      _    <- extendPath(edge)
    } yield edge

  def outE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, OUTGOING)

  def inE(edgeName: String): Traverser[Edge] =
    ontoE(edgeName, INCOMING)

  private def ontoE(edgeName: String, dir: Direction): Traverser[Edge] =
    for {
      State((head: Vertex) :: rest, _) <- getState
      edges = head.getRelationships(DynamicRelationshipType.withName(edgeName), dir)
      edge <- streamToTraverser(edges.toStream)
      _ <- extendPath(edge)
    } yield edge

  def outV(): Traverser[Vertex] =
    ontoV(OUTGOING)

  def inV(): Traverser[Vertex] =
    ontoV(INCOMING)

  private def ontoV(dir: Direction): Traverser[Vertex] =
    for {
      State((head: Edge) :: rest, _) <- getState
      v = if(dir == OUTGOING) head.getStartNode() else head.getEndNode()
      _ <- extendPath(v)
    } yield v

  def property[T](name: String): Traverser[T] = {
    for {
      State(head :: rest, _) <- getState
    } yield head.getProperty(name).asInstanceOf[T]
  }
}
*/