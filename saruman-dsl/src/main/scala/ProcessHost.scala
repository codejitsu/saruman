// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Defines host where the target process runs.
 */
sealed trait ProcessHost {
  def name: String

  def | (range: Range): ProcessHostGroup
}

case class ValidProcessHost(nameParts: collection.immutable.Seq[String]) extends ProcessHost {
  val name = nameParts.mkString(".")

  def | (range: Range): ProcessHostGroup = {
    val parts = nameParts.toList

    val hosts = range.map(r => parts.init :+ (parts.last + r.toString))

    ProcessHostGroup(hosts.map(host => ValidProcessHost(host)).toList)
  }
}

case class InvalidProcessHost(invalidPart: String, original: String) extends ProcessHost {
  def name: Nothing = throw new NoSuchElementException("InvalidProcessHost.name")

  def | (range: Range): Nothing = throw new NoSuchElementException("InvalidProcessHost.|")
}
