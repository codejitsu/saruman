// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Defines host where the target process runs.
 */
sealed trait ProcessHost {
  def name: String
}

case class ValidProcessHost(val nameParts: String*) extends ProcessHost {
  val name = nameParts.mkString(".")
}

case class InvalidProcessHost(val invalidPart: String) extends ProcessHost {
  def name: Nothing = throw new NoSuchElementException("InvalidProcessHost.name")
}
