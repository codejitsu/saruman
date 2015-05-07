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
case class Touch(hosts: Hosts, file: String)(implicit user: User) extends Task {
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
 * @param params command flags
 * @param user user
 */
class Rm(hosts: Hosts, target: String, params: List[String] = Nil)(implicit user: User) extends Task {
  private val rm: Processes = "rm" on hosts ~> {
    case Start => Exec("/bin/rm",  params ::: List(target) :_*)
  }

  private val rmTask: Task = rm ! Start

  override def run: TaskResult = rmTask()
}

object Rm {
  def apply(hosts: Hosts, target: String, params: List[String] = Nil)(implicit user: User): Rm =
    new Rm(hosts, target, params)(user)
}

/**
 * Remove file / dir task (ignore errors if target not exists).
 *
 * @param hosts target hosts
 * @param target file/dir to remove
 * @param user user
 */
case class RmIfExists(hosts: Hosts, target: String)(implicit user: User) extends Rm(hosts, target, List("-f"))(user)

/**
 * Copy file / dir task.
 *
 * @param hosts target hosts
 * @param source source object
 * @param destination destination object
 * @param params task flags
 * @param user user
 */
class Cp(hosts: Hosts, source: String, destination: String, params: List[String] = Nil)(implicit user: User) extends Task {
  private val rsync: Processes = "rsync" on hosts ~> {
    case Start => Exec("/usr/bin/rsync", source :: destination :: params :_*)
  }

  private val rsyncTask: Task = rsync ! Start

  override def run: TaskResult = rsyncTask()
}

object Cp {
  def apply(hosts: Hosts, source: String, destination: String, params: List[String])(implicit user: User): Cp =
    new Cp(hosts, source, destination, params)(user)

  def apply(hosts: Hosts, source: String, destination: String)(implicit user: User): Cp =
    new Cp(hosts, source, destination)(user)
}
