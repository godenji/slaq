package org.scalaquery.session

import java.sql.{PreparedStatement, Connection, DatabaseMetaData, Statement}
import org.scalaquery.Fail
import org.scalaquery.session.{
	ResultSetConcurrency => Concurrency,
	ResultSetHoldability => Holdability
}
import Concurrency._, ResultSetType._

/**
 * A database session which opens a connection and transaction on demand.
 */
trait Session extends java.io.Closeable { self =>
	
  def conn: Connection
  def metaData: DatabaseMetaData
  def capabilities: DatabaseCapabilities
  
  def cursorType: ResultSetType = ResultSetType.Auto
  def concurrencyType: Concurrency = Concurrency.Auto
  def holdabilityType: Holdability = Holdability.Auto

  final def prepareStatement(
  	sql: String,
		cursor: ResultSetType = ForwardOnly,
		concurrency: Concurrency = ReadOnly,
		holdability: Holdability = Holdability.Default): PreparedStatement = {
  	
    holdabilityType.withDefault(holdability) match {
      case Holdability.Default =>
        conn.prepareStatement(sql, cursorType.withDefault(cursor).intValue,
          concurrencyType.withDefault(concurrency).intValue)
      case h =>
        conn.prepareStatement(sql, cursorType.withDefault(cursor).intValue,
          concurrencyType.withDefault(concurrency).intValue,
          h.intValue)
    }
  }

  final def createStatement(
  	cursor: ResultSetType = ForwardOnly,
		concurrency: Concurrency = ReadOnly,
		holdability: Holdability = Holdability.Default): Statement = {
  	
    holdabilityType.withDefault(holdability) match {
      case Holdability.Default =>
        conn.createStatement(cursorType.withDefault(cursor).intValue,
          concurrencyType.withDefault(concurrency).intValue)
      case h =>
        conn.createStatement(cursorType.withDefault(cursor).intValue,
          concurrencyType.withDefault(concurrency).intValue,
          h.intValue)
    }
  }

  final def withPreparedStatement[T](
  	sql: String,
		cursor: ResultSetType = ForwardOnly,
		concurrency: Concurrency = ReadOnly,
		holdability: Holdability = Holdability.Default)(f: PreparedStatement => T): T = {
  	
    val st = prepareStatement(sql, cursor, concurrency, holdability)
    try f(st) finally st.close()
  }

  final def withStatement[T](
		cursor: ResultSetType = ForwardOnly,
		concurrency: Concurrency = ReadOnly,
		holdability: Holdability = Holdability.Default)(f: Statement => T): T = {
  	
    val st = createStatement(cursor, concurrency, holdability)
    try f(st) finally st.close()
  }

  def close(): Unit

  /**
   * Call this method within a <em>withTransaction</em> call to roll back 
   * the current transaction after <em>withTransaction</em> returns.
   */
  def rollback(): Unit

  /**
   * Run the supplied function within a transaction. If the function throws 
   * an Exception or the session's rollback() method is called, the transaction 
   * is rolled back, otherwise it is commited when the function returns.
   */
  def withTransaction[T](f: => T): T

  def forParameters(
		cursor: ResultSetType = cursorType, 
		concurrency: Concurrency = concurrencyType,
		holdability: Holdability = holdabilityType): Session = new Session {
  	
    override def cursorType = cursor
    override def concurrencyType = concurrency
    override def holdabilityType = holdability
    
    def conn = self.conn
    def metaData = self.metaData
    def capabilities = self.capabilities
    def close() = self.close()
    def rollback() = self.rollback()
    def withTransaction[T](f: => T) = self.withTransaction(f)
  }
}

class BaseSession private[session] (db: Database) extends Session {

  var open = false
  var doRollback = false
  var inTransaction = false

  lazy val conn = { open = true; db.createConnection() }
  lazy val metaData = conn.getMetaData()

  def capabilities = {
    val dc = db.capabilities
    if(dc ne null) dc
    else {
      val newDC = new DatabaseCapabilities(this)
      db.capabilities = newDC
      newDC
    }
  }

  def close() {
    if(open) conn.close()
  }

  def rollback() {
    if(conn.getAutoCommit) Fail(
    	"Cannot roll back session in auto-commit mode"
    )
    doRollback = true
  }

  def withTransaction[T](f: => T): T = if(inTransaction) f else {
    conn.setAutoCommit(false)
    inTransaction = true
    try {
      var done = false
      try {
        doRollback = false
        val res = f
        if(doRollback) conn.rollback()
        else conn.commit()
        done = true
        res
      } finally if(!done) conn.rollback()
    } finally {
      conn.setAutoCommit(true)
      inTransaction = false
    }
  }
}
