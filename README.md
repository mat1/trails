trails
======

Purely functional graph traversal combinators written in Scala.

trails is applying the idea of [parser combinators](http://en.wikipedia.org/wiki/Parser_combinator) to graph traversals.
The following combinators are supported:

    t1 ~ t2     // Sequence
    t1 | t2     // Choice
    t.?         // Optionality
    t.*         // Repetition 0..n
    t.+         // Repetition 1..n

##Features##

* Purely functional: No mutable state, no surprises!
* Lazy: Compute only as much information as required!
* Cycle aware: Avoid spinning in a circle!
* Supports different graph APIs ([blueprints](https://github.com/tinkerpop/blueprints/wiki) and [neo4j](http://www.neo4j.org/) are already included)

##Example##

    test("Cycles") {
      val graph = new TinkerGraph()
      val v0 = graph.addVertex("v0")

      val e0 = graph.addEdge("e0", v0, v0, "e")
      val f0 = graph.addEdge("f0", v0, v0, "f")
      val f1 = graph.addEdge("f1", v0, v0, "f")

      val expr0 = V("v0") ~ (out("e").+ ~ out("f")).+

      val traces = expr0.run(Env(graph))
      val paths = traces.map(t => t.path.reverse)

      assert(paths.size === 4)

      assert(paths.contains(List(v0, e0, v0, f1, v0)))
      assert(paths.contains(List(v0, e0, v0, f1, v0, e0, v0, f0, v0)))
      assert(paths.contains(List(v0, e0, v0, f0, v0)))
      assert(paths.contains(List(v0, e0, v0, f0, v0, e0, v0, f1, v0)))
    }


trails adapted many ideas (especially method names) from [gremlin](https://github.com/tinkerpop/gremlin/wiki).
