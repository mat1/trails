package ch.fhnw.imvs.trails.blueprint

import org.scalatest.FunSuite
import BlueprintTrails._

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.Vertex

class BlueprintTrailsTest extends FunSuite {


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

    val expr1 = V ~ out("e").* ~ out("f").? ~ (out("g")|in("e")).+ ~ out("e")

    seq(V(),many1(out("e")))

    val expr2 = for {
      v <- V
      oe1 <- many1(out("e"))
    } yield v

    val paths = Traverser.run(expr0, graph).map(_._1)

    assert(paths.size === 1)
    assert(paths.head === List(v0, e0, v1, f0, v1, g0, v1, e1, v2))
  }

  test("pets"){
    val graph = new TinkerGraph()
    val A = graph.addVertex("Alice"); A.setProperty("name", "Alice")
    val B = graph.addVertex("Bob"); B.setProperty("name", "Bob")
    val C = graph.addVertex("Carol"); C.setProperty("name", "Carol")
    val D = graph.addVertex("Dave"); D.setProperty("name", "Dave")

    val M = graph.addVertex("Murphy"); M.setProperty("name", "Murphy")
    val F = graph.addVertex("Fluffy"); F.setProperty("name", "Fluffy")

    val ER = "E"
    val FR = "F"
    val PET = "P"
    A.addEdge(ER,B)
    A.addEdge(FR,C)
    //B.addEdge(ER,A)
    B.addEdge(PET,M)
    C.addEdge(ER,B)
    C.addEdge(FR,D)
    D.addEdge(PET,F)


    val friends = V("Alice") ~> (out(ER)|out(FR)).+
    val pets = friends ~> out(PET) ^^ get[String]("name")

    val res = Traverser.run(pets, graph).map(_._2)
    println(res.take(5).distinct.mkString("\n"))
  }
}