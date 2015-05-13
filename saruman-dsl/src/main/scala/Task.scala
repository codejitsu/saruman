// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class TaskExecutionError(err: List[String]) extends Exception(err.mkString)

object VerbosityLevel extends Enumeration {
  type VerbosityLevel = Value
  val No, Verbose, Full = Value
}

import VerbosityLevel._

trait TaskM[+R] {
  self =>

  def run(verbose: VerbosityLevel = No): (Try[R], List[String], List[String])

  def apply(): (Try[R], List[String], List[String]) = run()

  def andThen[T >: R](task: TaskM[T]): TaskM[T] = this flatMap (_ => task)

  def description: String = ""

  def map[U](f: R => U): TaskM[U] = new TaskM[U] {
    override def run(verbose: VerbosityLevel = No): (Try[U], List[String], List[String]) = {
      val (selfRes, out, err) = self.run(verbose)
      (selfRes.map(f), out, err)
    }
  }

  def flatMap[T >: R](f: R => TaskM[T]): TaskM[T] = new TaskM[T] {
    override def run(verbose: VerbosityLevel = No): (Try[T], List[String], List[String]) = {
      val (selfRes, out, err) = self.run(verbose)

      selfRes match {
        case Success(r) =>
          val (nextRes, nout, nerr) = f(r).run(verbose)
          (nextRes, out ++ nout, err ++ nerr)

        case Failure(e) => (Failure(e), out, err)
      }
    }
  }
}

case class FailedTask(out: List[String], err: List[String]) extends TaskM[Boolean] {
  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = (Failure(new TaskExecutionError(Nil)), Nil, Nil)
}

case object EmptyTask extends TaskM[Boolean] {
  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = (Success(true), Nil, Nil)
}

class ShellTask(val ctx: Process, val op: Command)(implicit val user: User) extends TaskM[Boolean] {
  import scala.collection.mutable.ListBuffer
  import scala.sys.process._

  override def run(verbose: VerbosityLevel = No): (Try[Boolean], List[String], List[String]) = op match {
    case Start => execute(ctx.startCmd, verbose)

    case Stop => execute(ctx.stopCmd, verbose)

    case _ => ???
  }

  def doOut(out: ListBuffer[String], verbose: VerbosityLevel)(line: String): Unit = {
    out.append(line)

    verbose match {
      case Full => println(line)
      case _ =>
    }
  }

  private def executeLocal(cmd: CommandLine, verbose: VerbosityLevel): (Try[Boolean], List[String], List[String]) = user match {
    case lu @ LocalUser(_) =>
      val out = ListBuffer[String]()
      val err = ListBuffer[String]()

      verbose match {
        case Full => println(s"$op '${ctx.name}' (${cmd.cmd}) on '${ctx.host.toString}'")
        case _ =>
      }

      val result = cmd match {
        case SudoExec(_, _*) =>
          (s"echo '${lu.password().mkString}' | ${cmd.cmd}" run (ProcessLogger(doOut(out, verbose)(_),
            doOut(err, verbose)(_)))).exitValue()

        case Exec(_, _*) =>
          (cmd.cmd run (ProcessLogger(doOut(out, verbose)(_), doOut(err, verbose)(_)))).exitValue()

        case NoExec => 0
      }

      if (result == 0) {
        (Success(true), out.toList, err.toList)
      } else {
        (Failure(new TaskExecutionError(err.toList)), out.toList, err.toList)
      }

    case _ =>
      (Failure(new TaskExecutionError(List("Please provide localhost credentials."))), Nil,
        List("Please provide localhost credentials."))
  }

  private def buildSshCommandFor(remoteHost: Host, cmd: CommandLine, sshu: User with SshCredentials) = {
    val noHostKeyChecking = "-o" :: "UserKnownHostsFile=/dev/null" :: "-o" :: "StrictHostKeyChecking=no" :: Nil
    val keyFileArgs = sshu.keyFile.toList.flatMap("-i" :: _.getPath :: Nil)

    cmd match {
      case SudoExec(_, _*) =>
        "ssh" :: "-qtt" :: noHostKeyChecking ::: keyFileArgs ::: s"${sshu.username}@${remoteHost.toString()}" ::
          s"echo '${sshu.password().mkString}' | ${cmd.cmd}" :: Nil

      case Exec(_, _*) =>
        "ssh" :: "-qtt" :: noHostKeyChecking ::: keyFileArgs ::: s"${sshu.username}@${remoteHost.toString()}" ::
          cmd.cmd :: Nil

      case NoExec => Nil
    }
  }

  private def executeRemoteSsh(remoteHost: Host, cmd: CommandLine, verbose: VerbosityLevel): (Try[Boolean], List[String], List[String]) = {
    val out = ListBuffer[String]()
    val err = ListBuffer[String]()

    verbose match {
      case Full => println(s"$op '${ctx.name}' (${cmd.cmd}) on '${ctx.host.toString}'")
      case _ =>
    }

    def remoteCommandLine(user: User): List[String] = try {
      user match {
        case sshu: SshUser => buildSshCommandFor(remoteHost, cmd, sshu)
        case sshu: SshUserWithPassword => buildSshCommandFor(remoteHost, cmd, sshu)

        case _ => Nil
      }
    } catch {
      case NonFatal(e) => Nil
    }

    val commandLine = remoteCommandLine(user)

    verbose match {
      case Full => println(s"SSH: ${commandLine.mkString(" ")}")
      case _ =>
    }

    val proc = commandLine run (ProcessLogger(doOut(out, verbose)(_), doOut(err, verbose)(_)))
    val result = proc.exitValue

    if (result == 0) {
      (Success(true), out.toList, err.toList)
    } else {
      (Failure(new TaskExecutionError(err.toList)), out.toList, err.toList)
    }
  }

  private def execute(cmd: CommandLine, verbose: VerbosityLevel): (Try[Boolean], List[String], List[String]) = ctx.host match {
    case Localhost => executeLocal(cmd, verbose)
    case remoteHost: Host => executeRemoteSsh(remoteHost, cmd, verbose)
  }
}
