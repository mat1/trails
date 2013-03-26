package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import java.sql.ResultSet
import com.tinkerpop.blueprints.Vertex

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

    val paths = Traverser.paths(expr0, graph).take(3)

    println("hei")
println(paths.force)
    println("hei")

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

  //  val labels = Traverser.all(expr0, graph).force

   // println("labels")
   // println(labels.mkString("\n"))
  }

  test("transitive closure") {
    val graph = new TinkerGraph()
    val b0 = graph.addVertex("b0")

    val t0 = graph.addVertex("t0")
    val t1 = graph.addVertex("t1")
    val t2 = graph.addVertex("t2")

    val p0 = graph.addVertex("p0")
    val p1 = graph.addVertex("p1")
    val p2 = graph.addVertex("p2")
    val p3 = graph.addVertex("p3")
    val p4 = graph.addVertex("p4")
    val p5 = graph.addVertex("p5")

    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")


    graph.addEdge("changed0", b0, t1, "changed")

    graph.addEdge("select0", p0, t0, "select")
    graph.addEdge("select1", p0, t1, "select")
    graph.addEdge("select2", p1, t1, "select")
    graph.addEdge("select3", p2, t2, "select")
    graph.addEdge("select4", p3, t2, "select")

    graph.addEdge("calls0", p4, p0, "calls")
    graph.addEdge("calls1", p4, p1, "calls")
    graph.addEdge("calls2", p5, p1, "calls")
    graph.addEdge("calls3", p5, p2, "calls")
    graph.addEdge("calls4", p5, p3, "calls")

    graph.addEdge("uses0", v0, p0, "uses")
    graph.addEdge("uses1", v1, t0, "uses")

    def closure(trs: Traverser[Vertex]*): Traverser[Stream[Vertex]] =
      (trs.reduce((tr0, tr1) => choice[Vertex](tr0, tr1))).*


    val impactOfChangeOnView = V("b0") ~ out("changed") ~ closure(in("select"), in("calls")) ~> in("uses")

    closure(in("select"), in("calls")) ~> (in("select") | in("calls")).*



    //val labels = Traverser.all(impactOfChangeOnView, graph).force

   // println("labels")
   // println(labels.mkString("\n"))
  }

  test("order") {
    val graph = new TinkerGraph()
    val v0 = graph.addVertex("v0")
    val v1 = graph.addVertex("v1")
    val v2 = graph.addVertex("v2")
    val v3 = graph.addVertex("v3")
    val v4 = graph.addVertex("v4")
    val v5 = graph.addVertex("v5")
    val v6 = graph.addVertex("v6")

    graph.addEdge("e0", v0, v1, "e"); graph.addEdge("e1", v1, v2, "e")
    graph.addEdge("e2", v0, v3, "e")
    graph.addEdge("e3", v0, v4, "e"); graph.addEdge("e4", v4, v5, "e"); graph.addEdge("e5", v5, v6, "e")

    val expr0 = V("v0") ~ out("e").+ ^^ {case v0 ~ es => es.take(2)}

    val labels = Traverser.all(expr0, graph).take(10)

    println("labels")
    println(labels.mkString("\n"))

  }
}

