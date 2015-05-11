// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class TaskExecutionError(err: List[String]) extends Exception(err.mkString)

case class TaskIO(out: List[String], err: List[String])

trait TaskM[+R] {
  self =>

  def run: (Try[R], List[String], List[String])

  def apply(): (Try[R], List[String], List[String]) = run

  def andThen[T >: R](task: TaskM[T]): TaskM[T] = this flatMap (_ => task)

  def map[U](f: R => U): TaskM[U] = new TaskM[U] {
    override def run: (Try[U], List[String], List[String]) = {
      val (selfRes, out, err) = self()
      (selfRes.map(f), out, err)
    }
  }

  def flatMap[T >: R](f: R => TaskM[T]): TaskM[T] = new TaskM[T] {
    override def run: (Try[T], List[String], List[String]) = {
      val (selfRes, out, err) = self()

      selfRes match {
        case Success(r) =>
          val (nextRes, nout, nerr) = f(r).run
          (nextRes, out ++ nout, err ++ nerr)

        case Failure(e) => (Failure(e), out, err)
      }
    }
  }
}

case class FailedTask(out: List[String], err: List[String]) extends TaskM[Boolean] {
  override def run: (Try[Boolean], List[String], List[String]) = (Failure(new TaskExecutionError(Nil)), Nil, Nil)
}

case object EmptyTask extends TaskM[Boolean] {
  override def run: (Try[Boolean], List[String], List[String]) = (Success(true), Nil, Nil)
}

class ShellTask(val ctx: Process, val op: Command)(implicit val user: User) extends TaskM[Boolean] {
  import scala.collection.mutable.ListBuffer
  import scala.sys.process._

  override def run: (Try[Boolean], List[String], List[String]) = op match {
    case Start => execute(ctx.startCmd)

    case Stop => execute(ctx.stopCmd)

    case _ => ???
  }

  def doOut(out: ListBuffer[String])(line: String): Unit = {
    out.append(line)

    if(ctx.verbose) {
      println(line)
    }
  }

  private def executeLocal(cmd: CommandLine): (Try[Boolean], List[String], List[String]) = user match {
    case lu @ LocalUser(_) =>
      val out = ListBuffer[String]()
      val err = ListBuffer[String]()

      if (ctx.verbose) {
        println(s"$op '${ctx.name}' (${cmd.cmd}) on '${ctx.host.toString}'")
      }

      val result = cmd match {
        case SudoExec(_, _*) =>
          (s"echo '${lu.password().mkString}' | ${cmd.cmd}" run (ProcessLogger(doOut(out)(_), doOut(err)(_)))).exitValue()

        case Exec(_, _*) =>
          (cmd.cmd run (ProcessLogger(doOut(out)(_), doOut(err)(_)))).exitValue()

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

  private def executeRemoteSsh(remoteHost: Host, cmd: CommandLine): (Try[Boolean], List[String], List[String]) = {
    val out = ListBuffer[String]()
    val err = ListBuffer[String]()

    def remoteCommandLine(user: User): List[String] = try {
      user match {
        case sshu: SshUser =>
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
        case _ => Nil
      }
    } catch {
      case NonFatal(e) => Nil
    }

    val commandLine = remoteCommandLine(user)

    if (ctx.verbose) {
      println(s"SSH: ${commandLine.mkString(" ")}")
    }

    val proc = commandLine run (ProcessLogger(doOut(out)(_), doOut(err)(_)))
    val result = proc.exitValue

    if (result == 0) {
      (Success(true), out.toList, err.toList)
    } else {
      (Failure(new TaskExecutionError(err.toList)), out.toList, err.toList)
    }
  }

  private def execute(cmd: CommandLine): (Try[Boolean], List[String], List[String]) = ctx.host match {
    case Localhost => executeLocal(cmd)
    case remoteHost: Host => executeRemoteSsh(remoteHost, cmd)
  }
}
