// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.util.control.NonFatal

/**
 * Task.
 */
case class TaskResult(success: Boolean, out: List[String], err: List[String])

trait Task {
  self =>

  def run: TaskResult

  def apply(): TaskResult = run

  def andThen(task: Task): Task = new Task {
    override def run: TaskResult = {
      val thisResult = self.run
      val taskResult = task.run

      TaskResult(thisResult.success && taskResult.success, thisResult.out ++ taskResult.out,
        thisResult.err ++ taskResult.err)
    }
  }

  def map[U](f: Unit => U): Task = new Task {
    override def run: TaskResult = {
      self()
    }
  }

  def flatMap(f: TaskResult => Task): Task = {
    val selfRes = self()

    if (selfRes.success) {
      new Task {
        override def run: TaskResult = {
          if (selfRes.success) {
            val res = f(selfRes)()
            TaskResult(res.success, selfRes.out ++ res.out, selfRes.err ++ res.err)
          } else {
            selfRes
          }
        }
      }
    } else {
      FailedTask(selfRes.out, selfRes.err)
    }
  }
}

case class FailedTask(out: List[String], err: List[String]) extends Task {
  override def run: TaskResult = TaskResult(false, out, err)
}

class ShellTask(val ctx: Process, val op: Command)(implicit val user: User) extends Task {
  import scala.collection.mutable.ListBuffer
  import scala.sys.process._

  override def run: TaskResult = op match {
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

  private def executeLocal(cmd: CommandLine): TaskResult = user match {
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

      TaskResult(result == 0, out.toList, err.toList)

    case _ => TaskResult(false, Nil, List("Please provide localhost credentials."))
  }

  private def executeRemoteSsh(remoteHost: Host, cmd: CommandLine): TaskResult = {
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

    val proc = (commandLine run (ProcessLogger(doOut(out)(_), doOut(err)(_))))
    val result = proc.exitValue

    TaskResult(result == 0, out.toList, err.toList)
  }

  private def execute(cmd: CommandLine): TaskResult = ctx.host match {
    case Localhost => executeLocal(cmd)
    case remoteHost: Host => executeRemoteSsh(remoteHost, cmd)
  }
}
