// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import Dsl._

//All shell specific tasks go here

case class Touch(hosts: Hosts)(file: String)(implicit user: User) extends Task {
  private val touch: Processes = "touch" on hosts ~> {
    case Start => Exec("/usr/bin/touch", file)
  }

  private val touchTask: Task = touch ! Start

  override def run: TaskResult = touchTask()
}
