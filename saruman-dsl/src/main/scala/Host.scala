// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.collection.immutable.IndexedSeq

/**
 * Host.
 */
case class Host(parts: collection.immutable.Seq[HostPart]) {
  override def toString(): String = parts.map(_.toString).mkString(".")

  def ~ (part: String): Host = Host(parts :+ HostPart(part))

  def ~[T](part: IndexedSeq[T]): Hosts = {
    val appended = part.map(p => parts :+ HostPart(p.toString))

    val hostNames = appended.map(Host(_)).toSeq

    Hosts(hostNames)
  }

  def ~[T <: Product](part: T): Hosts = {
    val vals = for {
      i <- 0 until part.productArity
    } yield part.productElement(i).toString

    this ~ vals
  }

  def isValid: Boolean = parts.forall(_.isValid)
}

object Localhost extends Host(List(HostPart("localhost"))) {
  override def toString(): String = "localhost"

  override def ~ (part: String): Host = throw new IllegalArgumentException()

  override def ~[T](part: IndexedSeq[T]): Hosts = throw new IllegalArgumentException()

  override def ~[T <: Product](part: T): Hosts = throw new IllegalArgumentException()

  override def isValid: Boolean = true
}
