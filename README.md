trails [![Build Status](https://secure.travis-ci.org/danielkroeni/trails.png?branch=master)](http://travis-ci.org/danielkroeni/trails)
======

Purely functional graph traversal combinators written in Scala.

trails is applying the idea of [parser combinators](http://en.wikipedia.org/wiki/Parser_combinator) to graph traversals.
The following combinators are supported:
```scala
    t1 ~ t2     // Sequence
    t1 | t2     // Choice
    t.?         // Optionality
    t.*         // Repetition 0..n
    t.+         // Repetition 1..n
```

## Features

* Purely functional: No mutable state, no surprises!
* Lazy: Compute only as much information as required!
* Cycle aware: Avoid spinning in a circle!
* Supports different graph APIs ([blueprints](https://github.com/tinkerpop/blueprints/wiki) and [neo4j](http://www.neo4j.org/) are already included)

## Examples
```scala
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
```

Experimental table support:
```scala
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

      val table = Table(traces)
      println(table.pretty)
    }
```
Output:
```
    |--------------------------|------------|
    |out(e)                    |outE(e)     |
    |--------------------------|------------|
    |List([-e2- v2])           |List()      |
    |List([-e2- v2])           |List([-e3-])|
    |List([-e2- v2])           |List([-e4-])|
    |List([-e3- v3], [-e2- v2])|List()      |
    |List([-e4- v4], [-e2- v2])|List()      |
    |List([-e0- v1])           |List()      |
    |List([-e0- v1])           |List([-e1-])|
    |List([-e1- v3], [-e0- v1])|List()      |
    |--------------------------|------------|
```

## License
trails is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).


## Credits
trails adapted many ideas (especially method names) from [gremlin](https://github.com/tinkerpop/gremlin/wiki).
