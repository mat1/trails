package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import BlueprintTrails._

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.scalatest.matchers.{ShouldMatchers, MatchResult, Matcher}

class BlueprintTrailsTest extends FunSuite with ShouldMatchers {

  test("seq") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")

    val e0 = graph.addEdge("e0", v0, v1, "e")

    val t = V("v0") ~ outE("e") ~ inV

    val res = Traverser.run(t, graph)

    assert(res.size === 1)

    val (path, rv0 ~ re0 ~ rv1) = res.head

    assert(path === List(v0, e0, v1))
    assert(rv0 === v0)
    assert(re0 === e0)
    assert(rv1 === v1)
  }

  test("Example") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v0, v2, "f")

    val t = V("v0") ~ (outE("e")|outE("f")) ~ inV
    val res = Traverser.run(t, graph)

    assert(res.size === 2)

    val (List(`v0`, `e0`, `v1`), `v0` ~ `e0` ~ `v1`) = res.head
    val (List(`v0`, `e1`, `v2`), `v0` ~ `e1` ~ `v2`) = res.tail.head
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

    val paths = Traverser.run(expr0, graph).map(_._1)

    assert(paths.size === 1)
    assert(paths.head === List(v0, e0, v1, f0, v1, g0, v1, e1, v2))
  }
}