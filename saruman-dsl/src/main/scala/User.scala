// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * User
 */
trait User[+T] {
  def username: T
}

case object NoUser extends User[Nothing] {
  override def username: Nothing = throw new IllegalStateException("no username")
}

case class Username(username: String) extends User[String]

object User {
  implicit val DefaultUser: User[String] = NoUser
}
