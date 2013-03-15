package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import java.sql.ResultSet

class BlueprintTrailsTest extends FunSuite {
  import ch.fhnw.imvs.trails.Blueprint._


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

    val paths = Traverser.paths(expr0, graph)

    assert(paths.size === 1)
    assert(paths.head === List(v0, e0, v1, f0, v1, g0, v1, e1, v2))
  }

  test("Simple pattern II") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v1, v2, "e")


    val expr0 = V ~ out("e").+

    val paths = Traverser.paths(expr0, graph)

    assert(paths.size === 3)
    assert(paths contains List(v0, e0, v1))
    assert(paths contains List(v1, e1, v2))
    assert(paths contains List(v0, e0, v1, e1, v2))
  }

  test("Still lazy?") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")

    val e0 = graph.addEdge("e0", v0, v0, "e")

    val expr0 = V ~ out("e").+

    val paths = Traverser.paths(expr0, graph)

    assert(paths.size === 1)
    assert(paths contains List(v0, e0, v0))
  }

  test("option") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")

    val e0 = graph.addEdge("e0", v0, v0, "e")

    val expr0 = V ~ out("e") ~ optional(out("f")) ~ optional(out("e"))

    val paths = Traverser.paths(expr0, graph)

    assert(paths.size === 2)
    assert(paths contains List(v0, e0, v0))
    assert(paths contains List(v0, e0, v0, e0, v0))
  }

  test("choice") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")

    val e0 = graph.addEdge("e0", v0, v0, "e")

    val expr0 = V ~ out("e") ~> choice(out("f"),optional(out("e")))

    val pathsAndValues = Traverser.run(expr0, graph)

    assert(pathsAndValues.size === 2)
    assert(pathsAndValues contains (List(v0, e0, v0), None))
    assert(pathsAndValues contains (List(v0, e0, v0, e0, v0), Some(v0)))
  }

  test("many result") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")


    val e0 = graph.addEdge("e0", v0, v0, "e")


    val expr0 = V ~> out("e").*
    val pathsAndValues = Traverser.run(expr0, graph)

    assert(pathsAndValues.size === 2)
    assert(pathsAndValues contains (List(v0),List()))
    assert(pathsAndValues contains (List(v0,e0,v0),List(v0)))
  }

  test("label") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")
    val v3 = graph.addVertex("v3")

    val e0 = graph.addEdge("e0", v0, v1, "e")
    val e1 = graph.addEdge("e1", v1, v0, "e")
    val e2 = graph.addEdge("e2", v1, v3, "e")


    val expr0 = for {
      _ <- addLabel("X")(V())
      ns <- out("e").*
      l <- getLabel("X") //if l.map(_.head).inter
    } yield (l.map(_.head),ns.mkString("[",", ","]"))

    val labels = Traverser.all(expr0, graph).force

    println("labels")
    println(labels.mkString("\n"))
  }
}

