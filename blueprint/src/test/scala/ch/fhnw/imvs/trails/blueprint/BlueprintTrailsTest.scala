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


    val expr0 = V("v0") ~ outE("e").as("es") ~ inV().as("vs") ~ out("e")
    val traces = expr0.run(graph)

    val paths = traces.map(t => t._1.path.reverse)

    assert(paths.size === 2)

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

    val expr0 = V("v0") ~ out("e").as("out(e)").+ ~ outE("e").as("outE(e)").?
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

    val dbAction = from {
      (V("v0") ~ out("e").as("col1").+ ~ outE("e").as("col2").?).asTable("yeah")
    }.extract[Unit] (" select col1, col2 from yeah order by col1 desc ") { (rs: ResultSet) => printResultSet(rs) }

    dbAction(graph)
  }

  test("sql table properties") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    v0.setProperty("name", "Name0")
    v0.setProperty("age", 0)

    val v1 = graph.addVertex("v1")
    v1.setProperty("name", "Name1")
    v1.setProperty("age", 1)

    val v2 = graph.addVertex("v2")
    v2.setProperty("name", "Name2")
    v2.setProperty("age", 2)

    val v3 = graph.addVertex("v3")
    v3.setProperty("name", "Name3")
    v3.setProperty("age", 3)

    val v4 = graph.addVertex("v4")
    v4.setProperty("name", "Name4")
    v4.setProperty("age", 4)

    graph.addEdge("e0", v0, v1, "e")
    graph.addEdge("e1", v0, v2, "e")
    graph.addEdge("e2", v1, v3, "e")
    graph.addEdge("e3", v2, v4, "e")

    val sqlQuery = from (
      (V("v0") ~ out("e").^[String]("name") ~ out("e").^[Int]("age")).asTable("t1"),
      (V("v0") ~ out("e").^[String]("name") ~ out("e").^[Int]("age")).asTable("t2")
    ).extract (
      """
      select lower(t1.name) as lowerName, upper(t2.name)
        from t1, t2
       where t1.age != t2.age
      """
    ) ( printResultSet )

    sqlQuery(graph) // execute at the end
  }

    /*

 println("UNFOLD TABLE")

    Vector(// table
      Vector(List(a,b), List(c,d)) //row
    )
    =>

    Vector(
      Vector(List(a,b), c)
      Vector(List(a,b), d)
    )

    =>

    Vector(
      Vector(a,c)
      Vector(b,c)

      Vector(a,d)
      Vector(b,d)
    )
    */
}

