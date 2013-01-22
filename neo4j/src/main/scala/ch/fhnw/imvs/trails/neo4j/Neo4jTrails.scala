package ch.fhnw.imvs.trails.neo4j

import org.neo4j.graphdb.{GraphDatabaseService, PropertyContainer}
import ch.fhnw.imvs.trails.Trails

trait Neo4jTrails extends Trails {
  type Environment = GraphDatabaseService
  type PathElement = PropertyContainer
}
