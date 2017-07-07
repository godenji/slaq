package org.scalaquery.util

import scala.collection.mutable.ArrayBuffer
import org.scalaquery.session.PositionedParameters

final class SqlBuilder extends SqlBuilder.Segment { self =>
  import SqlBuilder._

  private val segments = new ArrayBuffer[Segment]
  private var currentStringSegment: StringSegment = null

  private def ss = {
    if (currentStringSegment eq null) {
      if (segments.isEmpty || segments.last.isInstanceOf[SqlBuilder]) {
        currentStringSegment = new StringSegment
        segments += currentStringSegment
      } else currentStringSegment = segments.last.asInstanceOf[StringSegment]
    }
    currentStringSegment
  }

  def +=(s: String) = { ss.sb append s; this }
  def +=(i: Int) = { ss.sb append i; this }
  def +=(l: Long) = { ss.sb append l; this }
  def +=(c: Char) = { ss.sb append c; this }
  def +=(s: SqlBuilder) = { ss.sb append s; this }

  def +?=(f: Setter) = { ss.setters append f; ss.sb append '?'; this }

  def sep[T](sequence: Traversable[T], separator: String)(f: T => Unit) {
    var first = true
    for (x <- sequence) {
      if (first) first = false else self += separator
      f(x)
    }
  }

  def isEmpty = ss.sb.isEmpty

  def createSlot = {
    val s = new SqlBuilder
    segments += s
    currentStringSegment = null
    s
  }

  def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]): Unit =
    for (s <- segments) s.appendTo(res, setters)

  def build = {
    val sb = new StringBuilder(64)
    val setters = new ArrayBuffer[Setter]
    appendTo(sb, setters)
    Result(sb.toString, new CombinedSetter(setters))
  }
}

object SqlBuilder {
  final type Setter = (PositionedParameters, Any) => Unit
  final case class Result(sql: String, setter: Setter)

  implicit class StringInterpolator(val sc: StringContext) extends AnyVal {
    @inline final def b(args: Any*)(implicit builder: SqlBuilder): SqlBuilder = {
      val (keys, vals) = (sc.parts.iterator, args.iterator)
      builder += keys.next
      while (keys.hasNext) {
        vals.next match {
          case s: String      => builder += s
          case i: Int         => builder += i
          case l: Long        => builder += l
          case c: Char        => builder += c
          case sb: SqlBuilder => builder += sb
        }
        builder += keys.next
      }
      builder
    }
  }

  private class CombinedSetter(b: Seq[Setter]) extends Setter {
    def apply(p: PositionedParameters, param: Any): Unit = for (s <- b) s(p, param)
  }

  trait Segment {
    def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]): Unit
  }

  class StringSegment extends Segment {
    val sb = new StringBuilder(32)
    val setters = new ArrayBuffer[Setter]

    def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]) {
      res append sb
      setters ++= this.setters
    }
  }
}
