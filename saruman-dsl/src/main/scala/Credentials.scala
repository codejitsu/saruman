// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import java.io.File

trait Credentials {
  def service: String
}

trait SshCredentials extends Credentials {
  val service = "ssh"
  def keyFile: Option[File]
}
