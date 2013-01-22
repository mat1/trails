package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import com.tinkerpop.blueprints.impls.tg.TinkerGraph

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
    val paths = traces.map(t => t.path.reverse)

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
    val paths = traces.map(t => t.path.reverse)

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
    val paths = traces.map(t => t.path.reverse)

    assert(paths.size === 1)
    assert(paths.head === List(v0))
  }
}

