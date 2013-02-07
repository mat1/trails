package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import java.sql.ResultSet

class BlueprintTrailsTest extends FunSuite {
  import ch.fhnw.imvs.trails.Blueprint._

  test("Cycles") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")

    val e0 = graph.addEdge("e0", v0, v0, "e")
    val f0 = graph.addEdge("f0", v0, v0, "f")
    val f1 = graph.addEdge("f1", v0, v0, "f")

    val expr0 = V("v0") ~ (out("e").+ ~ out("f")).+

    val traces = expr0.run(graph)
    val paths = traces.map(t => t._1.path.reverse)

    assert(paths.size === 4)

    assert(paths.contains(List(v0, e0, v0, f1, v0)))
    assert(paths.contains(List(v0, e0, v0, f1, v0, e0, v0, f0, v0)))
    assert(paths.contains(List(v0, e0, v0, f0, v0)))
    assert(paths.contains(List(v0, e0, v0, f0, v0, e0, v0, f1, v0)))
  }

  test("Simple pattern") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v1, v2, "e")

    val f0 = graph.addEdge("f0", v1, v1, "f")
    val g0 = graph.addEdge("g0", v1, v1, "g")


    val expr0 = V ~ out("e") ~ out("f") ~ out("g") ~ out("e")

    val traces = expr0.run(graph)
    val paths = traces.map(t => t._1.path.reverse)

    assert(paths.size === 1)
    assert(paths.head === List(v0, e0, v1, f0, v1, g0, v1, e1, v2))
  }

  test("matches") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v1, v2, "e")

    val f0 = graph.addEdge("f0", v1, v1, "f")
    val g0 = graph.addEdge("g0", v1, v1, "g")


    val expr0 = V ~ matches(out("e") ~ out("f") ~ out("g") ~ out("e"))
    val traces = expr0.run(graph)
    val paths = traces.map(t => t._1.path.reverse)

    assert(paths.size === 1)
    assert(paths.head === List(v0))
  }

  test("names") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")
    val v3 = graph.addVertex("v3")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v1, v3, "e")
    val e2 = graph.addEdge("e2", v0, v2, "e")
    val e3 = graph.addEdge("e3", v2, v3, "e")

    val f0 = graph.addEdge("f0", v1, v1, "f")


    val expr0 = V("v0") ~ outE("e").as[String]("es") ~ inV().as[String]("vs") ~ out("e")
    val traces = expr0.run(graph)

    val paths = traces.map(t => t._1.path.reverse)

    assert(paths.size === 2)

    val mergedNamedSubPaths = traces.foldLeft(Map[String, List[Path]]()){ case (acc, (Trace(_, _), namedSubPaths)) =>
      acc ++ namedSubPaths.map{ case (k,v) => (k.name, (v ++ acc.getOrElse(k.name,Nil))) }
    }

    assert(mergedNamedSubPaths.size === 2)
    assert(mergedNamedSubPaths.contains("es"))
    assert(mergedNamedSubPaths("es").size === 2)
    assert(mergedNamedSubPaths("es").contains(List(e0)))
    assert(mergedNamedSubPaths("es").contains(List(e2)))

    assert(mergedNamedSubPaths.contains("vs"))
    assert(mergedNamedSubPaths("vs").size === 2)
    assert(mergedNamedSubPaths("vs").contains(List(v1)))
    assert(mergedNamedSubPaths("vs").contains(List(v2)))
  }

  test("table") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")
    val v3 = graph.addVertex("v3")
    val v4 = graph.addVertex("v4")

    graph.addEdge("e0", v0, v1, "e")
    graph.addEdge("e1", v1, v3, "e")
    graph.addEdge("e2", v0, v2, "e")
    graph.addEdge("e3", v2, v3, "e")
    graph.addEdge("e4", v2, v4, "e")

    val expr0 = V("v0") ~ out("e").as[String]("out(e)").+ ~ outE("e").as[String]("outE(e)").?
    val traces = expr0.run(graph)

    val table = ScalaTable(traces)
    println(table.pretty)
  }

  test("sql table") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")
    val v3 = graph.addVertex("v3")
    val v4 = graph.addVertex("v4")

    graph.addEdge("e0", v0, v1, "e")
    graph.addEdge("e1", v1, v3, "e")
    graph.addEdge("e2", v0, v2, "e")
    graph.addEdge("e3", v2, v3, "e")
    graph.addEdge("e4", v2, v4, "e")

    val sqlQuery = fromTable("yeah") {
      V("v0") ~ out("e").as[String]("col1").+ ~ outE("e").as[String]("col2").?
    } extract {
      " select col1, col2 as COOL from yeah where col2 = 'List()' order by col1 desc "
    }

    val res: ResultSet = sqlQuery(graph)
  }
}

