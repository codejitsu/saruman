// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import Dsl._

//All shell specific tasks go here

/**
 * Create file task.
 *
 * @param hosts target hosts
 * @param file target file
 * @param user user
 */
case class Touch(hosts: Hosts)(file: String)(implicit user: User) extends Task {
  private val touch: Processes = "touch" on hosts ~> {
    case Start => Exec("/usr/bin/touch", file)
  }

  private val touchTask: Task = touch ! Start

  override def run: TaskResult = touchTask()
}

/**
 * Remove file / dir task.
 *
 * @param hosts target hosts
 * @param target file/dir to remove
 * @param user user
 */
case class Rm(hosts: Hosts)(target: String)(implicit user: User) extends Task {
  private val rm: Processes = "rm" on hosts ~> {
    case Start => Exec("/bin/rm", target)
  }

  private val rmTask: Task = rm ! Start

  override def run: TaskResult = rmTask()
}
