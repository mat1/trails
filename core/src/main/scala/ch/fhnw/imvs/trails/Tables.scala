package ch.fhnw.imvs.trails

import scalaz.Show


trait Tables {
  this: Trails =>

  /** Maps names to generated subpaths */
  type State = Map[String, List[Path]]

  /** Returns a traverser which stores the generated sub-paths in a map with the given name as the key.
    * @param name the name under which the sub-path is stored in the map
    * @param tr the traverser
    * @return a traverser which stores the generated sub-paths in a map
    */
  def name(name: String, tr: Traverser): Traverser =
    env => ts => {
      val (trace, state) = ts
      val size = trace.path.size
      tr(env)(ts).map { case (Trace(path, visitedPaths), namedSubPaths) =>
        val currentEvaluation = path.take(path.size - size)
        val updated = namedSubPaths.updated(name, currentEvaluation :: namedSubPaths.getOrElse(name, Nil))
        (Trace(path, visitedPaths), updated)
      }
    }

  implicit class TablesSyntax(t1: Traverser) {
    def as(n: String): Traverser = name(n, t1)
    def run(e: Environment) = t1(e)((Trace(Nil, None),Map()))
  }

  /**
   * A table representation of the named parts of a Stream of Traces.
   * @param traces the traces
   */
  case class ScalaTable(private val traces: Stream[(Trace, State)]) {
    val headers: Vector[String] = traces.foldLeft(Set[String]())((acc, t) => acc ++ t._2.keys).toVector.sorted
    def rows: Vector[Vector[List[Path]]] = traces.toVector.map(t => headers.map(h => t._2.getOrElse(h, Nil)))

    def pretty(implicit showPathElem: Show[PathElement]): String = {
      val colls = for((name, index) <- headers.zipWithIndex ) yield {
        val col = rows.map(row => row(index).map(Show[Path].shows).toString)
        val maxSize = col.map(_.size).max
        (name, maxSize, col)
      }

      val headerLine = colls.map { case (name, maxSize, _) => name.padTo(maxSize, " ").mkString }
      val data = for(i <- 0 until rows.size) yield {
        colls.map {case (name, maxSize, col) => col(i).padTo(maxSize, " ").mkString }
      }
      val separatorLine = colls.map { case (name, maxSize, _) => "-" * maxSize }
      (separatorLine +: headerLine +: separatorLine +: data :+ separatorLine).map(_.mkString("|","|","|")).mkString("\n")
    }
  }
}
