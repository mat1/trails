package ch.fhnw.imvs.trails

import scala.language.existentials
import scalaz.Show
import reflect.ClassTag
import java.sql.{DriverManager, Connection, ResultSet}


trait Tables {
  this: Trails =>

  case class Named(name: String, tag: ClassTag[_])

  /** Maps names to generated subpaths */
  type State = Map[Named, List[Path]]

  /** Returns a traverser which stores the generated sub-paths in a map with the given name as the key.
    * @param name the name under which the sub-path is stored in the map
    * @param tr the traverser
    * @return a traverser which stores the generated sub-paths in a map
    */
  def name(name: String, tr: Traverser, tag: ClassTag[_]): Traverser =
    env => ts => {
      val (trace, state) = ts
      val size = trace.path.size
      tr(env)(ts).map { case (Trace(path, visitedPaths), namedSubPaths) =>
        val currentEvaluation = path.take(path.size - size)
        val key = Named(name, tag)
        val updated = namedSubPaths.updated(key, currentEvaluation :: namedSubPaths.getOrElse(key, Nil))
        (Trace(path, visitedPaths), updated)
      }
    }

  def fromTable(tableName: String)(traverser: Traverser): Extract = new Extract(tableName, traverser)

  class Extract(tableName: String, traverser: Traverser) {
    def extract(query: String): Environment => ResultSet = e => {
      val travRes = traverser.run(e)
      val tab = ScalaTable(travRes)
      println("headers: " + tab.headers.map(h => h.name + " " + h.tag.runtimeClass.getSimpleName))


      Class.forName("org.hsqldb.jdbc.JDBCDriver")


      //name varchar(10), city varchar(10), phone integer
      def prepareTable(meta: Vector[Named], data: Seq[Seq[Any]]): Connection = {
        val connection = DriverManager.getConnection("jdbc:hsqldb:mem:dsl;shutdown=true","SA","")
        val schemaStmt = connection.createStatement()
        val schemaSql = s"create memory table $tableName(${meta.map(m => m.name + " varchar(100)" /*+ m._2*/).mkString(", ")})"
        println(schemaSql)
        schemaStmt.executeUpdate(schemaSql)

        val sql = s"insert into $tableName (${meta.map(_.name).mkString(", ")}) values (${meta.map(_ => "?").mkString(", ")})"
        println(sql)
        val ps = connection.prepareStatement(sql)

        val types = meta.map(_.tag)

        for (d <- data) {
          for (((t,v), i) <- types.zip(d).zipWithIndex) {
            //t match {
            ps.setString(i + 1, ""+v)
          //    case "varchar(10)" =>
          //    case "integer" => ps.setInt(i + 1, v.asInstanceOf[Int])
          //    case _ => throw new IllegalArgumentException("Unknown type: " + t)
          //  }
          }
          ps.addBatch()
        }

        ps.executeBatch()
        ps.close()

        connection
      }

      val con = prepareTable(tab.headers, tab.rows)

      val stmt =  con.createStatement()
      val rs = stmt.executeQuery(query)

      printResultSet(rs)

      rs.close()
      stmt.close()
      con.close()


      null
    }
  }

  def printResultSet(result: ResultSet) {
    val meta = result.getMetaData
    val colCount = meta.getColumnCount()

    val columnNames = for(i <- 1 to colCount) yield meta.getColumnName(i)
    println(columnNames.mkString(" | "))

    val builder = List.newBuilder[IndexedSeq[Any]]
    while(result.next()) {
      builder += (for (i <- 1 to colCount) yield result.getString(i))
    }

    for (d <- builder.result()) {
      println(d.mkString(" | "))
    }
  }

  implicit class TablesSyntax(t1: Traverser) {
    def as[T: ClassTag](n: String): Traverser = name(n, t1, implicitly[ClassTag[T]])
    def run(e: Environment) = t1(e)((Trace(Nil, None),Map()))
  }

  /**
   * A table representation of the named parts of a Stream of Traces.
   * @param traces the traces
   */
  case class ScalaTable(private val traces: Stream[(Trace, State)]) {

    val headers: Vector[Named] = traces.foldLeft(Set[Named]())((acc, t) => acc ++ t._2.keys).toVector.sortBy(_.name)
    def rows: Vector[Vector[List[Path]]] = traces.toVector.map(t => headers.map(h => t._2.getOrElse(h, Nil)))

    def pretty(implicit showPathElem: Show[PathElement]): String = {
      val colls = for((name, index) <- headers.zipWithIndex ) yield {
        val col = rows.map(row => row(index).map(Show[Path].shows).toString)
        val maxSize = col.map(_.size).max
        (name, maxSize, col)
      }

      val headerLine = colls.map { case (named, maxSize, _) => named.name.padTo(maxSize, " ").mkString }
      val data = for(i <- 0 until rows.size) yield {
        colls.map {case (name, maxSize, col) => col(i).padTo(maxSize, " ").mkString }
      }
      val separatorLine = colls.map { case (name, maxSize, _) => "-" * maxSize }
      (separatorLine +: headerLine +: separatorLine +: data :+ separatorLine).map(_.mkString("|","|","|")).mkString("\n")
    }
  }
}
