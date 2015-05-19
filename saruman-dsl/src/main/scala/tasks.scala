// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import net.codejitsu.saruman.dsl.Dsl._
import net.codejitsu.saruman.dsl.VerbosityLevel.VerbosityLevel
import VerbosityLevel._

import scala.util.Try

//All shell specific tasks go here

/**
 * Create file task.
 *
 * @param hosts target hosts
 * @param target target file
 * @param usingSudo true, if task have to be started with sudo
 * @param usingPar true, if parallel ececution required.
 * @param user user
 */
case class Touch(hosts: Hosts, target: String,
                 usingSudo: Boolean = false, usingPar: Boolean = false)(implicit user: User)
  extends TaskM[Boolean] with UsingSudo[Touch] with UsingParallelExecution[Touch] {

  private val touch: Processes = "touch" on hosts ~> {
    case Start => if (usingSudo) {
      Sudo ~ Exec("/usr/bin/touch", target)
    } else{
      Exec("/usr/bin/touch", target)
    }
  }

  private val touchTask: TaskM[Boolean] = if (usingPar) {
    touch !! Start
  } else {
    touch ! Start
  }

  override def description: String = "create"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description '${target}' " +
          s"on ${hosts.hosts.head.toString()} (${hosts.hosts.size} hosts})")
      case _ =>
    }

    touchTask.run(verbose)
  }

  override def sudo: Touch = this.copy(usingSudo = true)

  override def par: Touch = this.copy(usingPar = true)
}

/**
 * Remove file / dir task.
 *
 * @param hosts target hosts
 * @param target file/dir to remove
 * @param params command flags
 * @param usingSudo true, if task have to be started with sudo
 * @param usingPar true, if parallel ececution required.
 * @param user user
 */
class Rm(hosts: Hosts, target: String, params: List[String] = Nil,
         usingSudo: Boolean = false, usingPar: Boolean = false)(implicit user: User)
  extends TaskM[Boolean] with UsingSudo[Rm] with UsingParallelExecution[Rm] {

  private val rm: Processes = "rm" on hosts ~> {
    case Start => if(usingSudo) {
      Sudo ~ Exec("/bin/rm",  params ::: List(target) :_*)
    } else {
      Exec("/bin/rm",  params ::: List(target) :_*)
    }
  }

  private val rmTask: TaskM[Boolean] = if(usingPar) {
    rm !! Start
  } else {
    rm ! Start
  }

  override def description: String = "remove file(s)"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val h = if (hosts.hosts.nonEmpty) {
          "(and " + (hosts.hosts.size - 1) + " other hosts)"
        } else {
          ""
        }

        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description '${target}' on ${hosts.hosts.head.toString()} $h")
      case _ =>
    }

    val result = rmTask.run(verbose)

    verbose match {
      case Verbose | Full => println("--------------------------------------------------------------")
      case _ =>
    }

    result
  }

  override def sudo: Rm = Rm(hosts, target, params, true, usingPar)

  override def par: Rm = Rm(hosts, target, params, usingSudo, true)
}

object Rm {
  def apply(hosts: Hosts, target: String, params: List[String] = Nil, usingSudo: Boolean = false, usingPar: Boolean = false)(implicit user: User): Rm =
    new Rm(hosts, target, params, usingSudo, usingPar)(user)
}

/**
 * Remove file / dir task (ignore errors if target not exists).
 *
 * @param hosts target hosts
 * @param target file/dir to remove
 * @param usingSudo true, if task have to be started with sudo
 * @param user user
 */
case class RmIfExists(hosts: Hosts, target: String, usingSudo: Boolean = false,
                      usingPar: Boolean = false)(implicit user: User) extends Rm(hosts, target, List("-f"), usingSudo, usingPar)(user) {
  override def description: String = "remove file(s) (if exists)"
}

/**
 * Copy file / dir task.
 *
 * @param hosts target hosts
 * @param source source object
 * @param destination destination object
 * @param params task flags
 * @param usingSudo true, if task have to be started with sudo
 * @param usingPar true, if parallel ececution required.
 * @param user user
 */
class Cp(hosts: Hosts, source: String, destination: String,
         params: List[String] = Nil, usingSudo: Boolean = false,
         usingPar: Boolean = false)(implicit user: User)
  extends TaskM[Boolean] with UsingSudo[Cp] with UsingParallelExecution[Cp] {

  private val rsync: Processes = "rsync" on hosts ~> {
    case Start => if(usingSudo) {
      Sudo ~ Exec("/usr/bin/rsync", params ::: List(source, destination): _*)
    } else {
      Exec("/usr/bin/rsync", params ::: List(source, destination): _*)
    }
  }

  private val rsyncTask: TaskM[Boolean] = if (usingPar) {
    rsync !! Start
  } else {
    rsync ! Start
  }

  override def description: String = "copy file(s)"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val h = if (hosts.hosts.nonEmpty) {
          "(and " + (hosts.hosts.size - 1) + " other hosts)"
        } else {
          ""
        }

        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description '${source}' on ${hosts.hosts.head.toString()} $h")
      case _ =>
    }

    val result = rsyncTask.run(verbose)

    verbose match {
      case Verbose | Full => println("--------------------------------------------------------------")
      case _ =>
    }

    result
  }

  override def sudo: Cp = Cp(hosts, source, destination, params, true, usingPar)

  override def par: Cp = Cp(hosts, source, destination, params, usingSudo, true)
}

object Cp {
  def apply(hosts: Hosts, source: String, destination: String, params: List[String] = Nil,
            sudo: Boolean = false, parallel: Boolean = false)(implicit user: User): Cp =
    new Cp(hosts, source, destination, params, sudo, parallel)(user)

  def apply(hosts: Hosts, source: String, destination: String)(implicit user: User): Cp = Cp(hosts, source, destination, Nil)
}

/**
 * Upload file to remote host(s).
 *
 * @param source source file to upload
 * @param target destination hosts
 * @param destinationPath path on destination hosts
 * @param usingSudo true, if task have to be started with sudo
 * @param usingPar true, if parallel ececution required.
 * @param user user
 */
case class Upload(target: Hosts, source: String, destinationPath: String,
                  usingSudo: Boolean = false, usingPar: Boolean = false)(implicit user: LocalUser)
  extends TaskM[Boolean] with UsingSudo[Upload] with UsingParallelExecution[Upload] {

  private lazy val uploadProcs = target.hosts map {
    case h: Host =>
      val up: Process = "rsync" on Localhost ~> {
        case Start => if (usingSudo) {
          Sudo ~ Exec("/usr/bin/rsync", "-avzhe", "ssh", source, s"${h.toString()}:$destinationPath")
        } else {
          Exec("/usr/bin/rsync", "-avzhe", "ssh", source, s"${h.toString()}:$destinationPath")
        }
      }

      up
  }

  private lazy val uploadTask: TaskM[Boolean] = if (usingPar) {
    Processes(uploadProcs) !! Start
  } else {
    Processes(uploadProcs) ! Start
  }

  override def description: String = "upload file(s)"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val h = if (target.hosts.nonEmpty) {
          "(and " + (target.hosts.size - 1) + " other hosts)"
        } else {
          ""
        }

        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description '${source}' to ${target.hosts.head.toString()} $h")
      case _ =>
    }

    val result = uploadTask.run(verbose)

    verbose match {
      case Verbose | Full => println("--------------------------------------------------------------")
      case _ =>
    }

    result
  }

  override def sudo: Upload = this.copy(usingSudo = true)

  override def par: Upload = this.copy(usingPar = true)
}

/**
 * Stop tomcat service.
 *
 * @param hosts hosts.
 * @param usingSudo true, if sudo needed.
 * @param usingPar true, if parallel ececution required.
 * @param user user.
 */
case class StopTomcat(hosts: Hosts, usingSudo: Boolean = false,
                      usingPar: Boolean = false)(implicit user: User)
  extends TaskM[Boolean] with UsingSudo[StopTomcat] with UsingParallelExecution[StopTomcat] {

  private val tomcats: Processes = "tomcat" on hosts ~> {
    case Stop => if(usingSudo) {
      Sudo ~ Exec("/etc/init.d/tomcat7", "stop")
    } else {
      Exec("/etc/init.d/tomcat7", "stop")
    }
  }

  private val tomcatsTask: TaskM[Boolean] = if (usingPar) {
    tomcats !! Stop
  } else {
    tomcats ! Stop
  }

  override def description: String = "stop tomcat service"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val h = if (hosts.hosts.nonEmpty) {
          "(and " + (hosts.hosts.size - 1) + " other hosts)"
        } else {
          ""
        }

        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description on ${hosts.hosts.head.toString()} $h")
      case _ =>
    }

    val result = tomcatsTask.run(verbose)

    verbose match {
      case Verbose | Full => println("--------------------------------------------------------------")
      case _ =>
    }

    result
  }

  override def sudo: StopTomcat = this.copy(usingSudo = true)

  override def par: StopTomcat = this.copy(usingPar = true)
}

/**
 * Start tomcat service.
 *
 * @param hosts hosts.
 * @param usingSudo true, if sudo needed.
 * @param usingPar true, if parallel ececution required.
 * @param user user.
 */
case class StartTomcat(hosts: Hosts, usingSudo: Boolean = false,
                       usingPar: Boolean = false)(implicit user: User)
  extends TaskM[Boolean] with UsingSudo[StartTomcat] with UsingParallelExecution[StartTomcat] {

  private val tomcats: Processes = "tomcat" on hosts ~> {
    case Start => if(usingSudo) {
      Sudo ~ Exec("/etc/init.d/tomcat7", "start")
    } else {
      Exec("/etc/init.d/tomcat7", "start")
    }
  }

  private val tomcatsTask: TaskM[Boolean] = if (usingPar) {
    tomcats !! Start
  } else {
    tomcats ! Start
  }

  override def description: String = "start tomcat service"

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = {
    verbose match {
      case Verbose | Full =>
        val h = if (hosts.hosts.nonEmpty) {
          "(and " + (hosts.hosts.size - 1) + " other hosts)"
        } else {
          ""
        }

        val withSudo = if(usingSudo) {
          s"${Console.GREEN}sudo${Console.WHITE}"
        } else {
          ""
        }

        val withPar = if(usingPar) {
          s"${Console.GREEN}!!${Console.WHITE}"
        } else {
          ""
        }

        println(s"[ ${Console.YELLOW}*${Console.WHITE} $withSudo $withPar] $description on ${hosts.hosts.head.toString()} $h")
      case _ =>
    }

    val result = tomcatsTask.run(verbose)

    verbose match {
      case Verbose | Full => println("--------------------------------------------------------------")
      case _ =>
    }

    result
  }

  override def sudo: StartTomcat = this.copy(usingSudo = true)

  override def par: StartTomcat = this.copy(usingPar = true)
}
