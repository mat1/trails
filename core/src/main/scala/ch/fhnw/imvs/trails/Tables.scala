package ch.fhnw.imvs.trails

import scala.language.existentials
import scalaz.Show
import reflect.ClassTag
import java.sql.{DriverManager, Connection, ResultSet}

/*
trait Tables {
  this: Trails =>

  case class Named(name: String, tag: ClassTag[_])

  /** Maps names to generated subpaths */
  type State = Map[Named, List[Any]]


  def namedSubPath(keyName: String, tr: Traverser[_]): Traverser =
    name[Path](keyName, tr)((p: Path) => p)

  /** Returns a traverser which stores the generated sub-paths in a map with the given name as the key.
    * @param name the name under which the sub-path is stored in the map
    * @param tr the traverser
    * @return a traverser which stores the generated sub-paths in a map
    */
  def name[A: ClassTag](name: String, tr: Traverser)(select: Path => A = (x: Path) => x.asInstanceOf[A]): Traverser =
    env => ts => {
      val (trace, state) = ts
      val size = trace.path.size
      tr(env)(ts).map { case (Trace(path, visitedPaths), namedSubPaths) =>
        val currentEvaluation = path.take(path.size - size)
        val key = Named(name, implicitly[ClassTag[A]])
        val updated = namedSubPaths.updated(key, select(currentEvaluation) :: namedSubPaths.getOrElse(key, Nil))
        (Trace(path, visitedPaths), updated)
      }
    }

  def pathTable(tableName: String, traverser: Traverser): Environment => Connection => Connection = env => connection => {
    val travRes = traverser.run(env)
    val tab = ScalaTable(travRes)
    println("headers: " + tab.headers.map(h => h.name + " " + h.tag.runtimeClass.getSimpleName))

    def tagToSqlType(tt: ClassTag[_]): String = tt match {
      case tt if tt == ClassTag[String](classOf[String]) => "varchar(100)"
      case tt if tt == ClassTag.Int => "integer"
      case tt => "varchar(100)" // throw new IllegalArgumentException("Unknown type: " + tt)
    }

    def prepareTable(table: ScalaTable): Connection = {
      val meta = table.headers
      val data = table.rows
      val schemaStmt = connection.createStatement()

      val schemaSql = s"create memory table $tableName(${meta.map(m => m.name + " " + tagToSqlType(m.tag)).mkString(", ")})"
      println(schemaSql)
      schemaStmt.executeUpdate(schemaSql)

      val sql = s"insert into $tableName (${meta.map(_.name).mkString(", ")}) values (${meta.map(_ => "?").mkString(", ")})"
      println(sql)
      val ps = connection.prepareStatement(sql)

      val tags = meta.map(_.tag)

      for (d <- data) {
        for (((t,v), i) <- tags.zip(d).zipWithIndex) {
          t match {
            case tt if tt == ClassTag[String](classOf[String]) => ps.setString(i + 1, v.headOption.map(_.toString).getOrElse(null) )
            case tt if tt == ClassTag.Int => ps.setObject(i + 1, v.headOption.map(_.asInstanceOf[Int]).getOrElse(null))
            case tt=> ps.setString(i + 1, v.headOption.map(_.toString.take(100)).getOrElse(null) ) // throw new IllegalArgumentException("Unknown type: " + t)
          }
        }
        ps.addBatch()
      }

      ps.executeBatch()
      ps.close()

      connection
    }
    prepareTable(tab)
  }

  def from(first: Environment => Connection => Connection, inMemDb: (Environment => Connection => Connection)*): Environment => Connection = env => {
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    val connection = DriverManager.getConnection("jdbc:hsqldb:mem:dsl;shutdown=true","SA","")
    inMemDb.foldLeft(first(env)(connection))((fs, f) => f(env)(fs))
  }

  def processTable[T](dbF: Environment => Connection, query: String, resultF: ResultSet => T): Environment => T = env => {
    val conn = dbF(env)
    val stmt =  conn.createStatement()
    val rs = stmt.executeQuery(query)
    val res = resultF(rs)

    rs.close()
    stmt.close()
    conn.close()

    res
  }

  implicit class ConnectionSyntax(dbf: Environment => Connection) {
    def extract[T](query: String)(resultF: ResultSet => T ): Environment => T = processTable[T](dbf, query, resultF)
  }

  implicit class TablesSyntax(t1: Traverser) {
    // Much better idea: Use "AST construction style" like scala parser combinators ^^ t1 ~ t2 => t1.prop("a")
    def as(n: String): Traverser = namedSubPath(n, t1)

    def asTable(name: String): Environment => Connection => Connection = pathTable(name, t1)
    def run(e: Environment) = t1(e)((Trace(Nil, None),Map()))
  }

  def printResultSet(result: ResultSet) {
    val meta = result.getMetaData
    val colCount = meta.getColumnCount()

    val columnNames = for(i <- (1 to colCount).toList) yield meta.getColumnLabel(i) //TODO alias names
    println(columnNames.mkString(" | "))

    val builder = List.newBuilder[IndexedSeq[Any]]
    while(result.next()) {
      builder += (for (i <- 1 to colCount) yield (result.getString(i)))
    }

    for (d <- builder.result()) {
      println(d.mkString(" | "))
    }
  }

  /**
   * A table representation of the named parts of a Stream of Traces.
   * @param traces the traces
   */
  case class ScalaTable(private val traces: Stream[(Trace, State)]) {

    val headers: Vector[Named] = traces.foldLeft(Set[Named]())((acc, t) => acc ++ t._2.keys).toVector.sortBy(_.name)
    def rows: Vector[Vector[List[Any]]] = traces.toVector.map(t => headers.map(h => t._2.getOrElse(h, Nil)))

    def pretty(implicit showPathElem: Show[PathElement]): String = {
      val colls = for((name, index) <- headers.zipWithIndex ) yield {
        val col = rows.map(row => row(index).toString)
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
*/