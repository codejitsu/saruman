// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import net.codejitsu.saruman.dsl.Dsl._

import scala.util.Try

//All shell specific tasks go here

/**
 * Create file task.
 *
 * @param hosts target hosts
 * @param file target file
 * @param user user
 */
case class Touch(hosts: Hosts, file: String)(implicit user: User) extends TaskM[Boolean] {
  private val touch: Processes = "touch" on hosts ~> {
    case Start => Exec("/usr/bin/touch", file)
  }

  private val touchTask: TaskM[Boolean] = touch ! Start

  override def run: (Try[Boolean], List[String], List[String]) = touchTask()
}

/**
 * Remove file / dir task.
 *
 * @param hosts target hosts
 * @param target file/dir to remove
 * @param params command flags
 * @param user user
 */
class Rm(hosts: Hosts, target: String, params: List[String] = Nil)(implicit user: User) extends TaskM[Boolean] {
  private val rm: Processes = "rm" on hosts ~> {
    case Start => Exec("/bin/rm",  params ::: List(target) :_*)
  }

  private val rmTask: TaskM[Boolean] = rm ! Start

  override def run: (Try[Boolean], List[String], List[String]) = rmTask()
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
class Cp(hosts: Hosts, source: String, destination: String, params: List[String] = Nil)(implicit user: User) extends TaskM[Boolean] {
  private val rsync: Processes = "rsync" on hosts ~> {
    case Start => Exec("/usr/bin/rsync", params ::: List(source, destination) :_*)
  }

  private val rsyncTask: TaskM[Boolean] = rsync ! Start

  override def run: (Try[Boolean], List[String], List[String]) = rsyncTask()
}

object Cp {
  def apply(hosts: Hosts, source: String, destination: String, params: List[String])(implicit user: User): Cp =
    new Cp(hosts, source, destination, params)(user)

  def apply(hosts: Hosts, source: String, destination: String)(implicit user: User): Cp =
    new Cp(hosts, source, destination)(user)
}

/**
 * Upload file to remote host(s).
 *
 * @param source source file to upload
 * @param target destination hosts
 * @param destinationPath path on destination hosts
 * @param user user
 */
case class Upload(source: String, target: Hosts, destinationPath: String)(implicit user: LocalUser) extends TaskM[Boolean] {
  private lazy val uploadTasks = target.hosts map {
    case h: Host =>
      val up: Process = "rsync" on Localhost ~> {
        case Start => Exec("/usr/bin/rsync", "-avzhe", "ssh", source, s"${h.toString()}:$destinationPath")
      }

      up ! Start
  }

  private lazy val uploadTask: TaskM[Boolean] = uploadTasks.foldLeft[TaskM[Boolean]](EmptyTask)((acc, t) => acc flatMap (_ => t))

  override def run: (Try[Boolean], List[String], List[String]) = uploadTask()
}
