// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import java.io.File

/**
 * User
 */
sealed trait User {
  def username: String
}

case object NoUser extends User {
  override def username: String = throw new IllegalStateException("no username")
}

case class SshUser(username: String, keyFile: Option[File]) extends User with SshCredentials {
  lazy val passphrase = System.console.readLine("Please enter your passphrase:")
}

object User {
  implicit val DefaultUser: User = NoUser
}
