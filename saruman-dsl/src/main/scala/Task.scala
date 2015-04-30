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

  def andThen(task: Task): Task = new Task {
    override def run: TaskResult = {
      val thisResult = self.run
      val taskResult = task.run

      TaskResult(thisResult.success && taskResult.success, thisResult.out ++ taskResult.out,
        thisResult.err ++ taskResult.err)
    }
  }
}

class RunnableTask(val ctx: Process, val user: User, val op: Command) extends Task {
  import scala.collection.mutable.ListBuffer
  import scala.sys.process._

  override def run: TaskResult = op match {
    case Start => execute(ctx.startCmd.cmd)

    case Stop => execute(ctx.stopCmd.cmd)

    case _ => ???
  }

  def doOut(out: ListBuffer[String])(line: String): Unit = {
    out.append(line)

    if(ctx.verbose) {
      println(line)
    }
  }

  private def executeLocal(cmd: String): TaskResult = user match {
    case lu @ LocalUser(_) =>
      val out = ListBuffer[String]()
      val err = ListBuffer[String]()

      if (ctx.verbose) {
        println(s"$op '${ctx.name}' (${cmd.trim}) on '${ctx.host.toString}'")
      }

      val result = (s"echo '${lu.password().mkString}' | $cmd" run (ProcessLogger(doOut(out)(_), doOut(err)(_)))).exitValue()

      TaskResult(result == 0, out.toList, err.toList)

    case _ => TaskResult(false, Nil, List("Please provide localhost credentials."))
  }

  private def executeRemoteSsh(remoteHost: Host, cmd: String): TaskResult = {
    val out = ListBuffer[String]()
    val err = ListBuffer[String]()

    def remoteCommandLine(user: User): List[String] = try {
      user match {
        case sshu: SshUser =>
          val noHostKeyChecking = "-o" :: "UserKnownHostsFile=/dev/null" :: "-o" :: "StrictHostKeyChecking=no" :: Nil
          val keyFileArgs = sshu.keyFile.toList.flatMap("-i" :: _.getPath :: Nil)

          "ssh" :: "-qtt" :: noHostKeyChecking ::: keyFileArgs ::: s"${sshu.username}@${remoteHost.toString()}" :: s"echo '${sshu.password().mkString}' | $cmd" :: Nil
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

  private def executeRemoteDirectSsh(remoteHost: Host, cmd: String): TaskResult = user match {
    case sshu: SshUser =>
      import com.decodified.scalassh.{SimplePasswordProducer, PublicKeyLogin, SSH}
      import com.decodified.scalassh.PublicKeyLogin.DefaultKeyLocations

      val out = ListBuffer[String]()
      val err = ListBuffer[String]()

      val publicKeyLogin =
        PublicKeyLogin(user.username, SimplePasswordProducer(sshu.password().mkString),
          sshu.keyFile map (_.getPath :: Nil) getOrElse DefaultKeyLocations)

      val result = SSH(remoteHost.toString(), publicKeyLogin) { ssh =>
        ssh.exec(cmd)
      }

      result match {
        case Left(e) => TaskResult(false, Nil, List(e))
        case Right(b) => TaskResult(true, List(b.stdOutAsString()), List(b.stdErrAsString()))
      }

    case _ => TaskResult(false, Nil, List("Please provide ssh credentials."))
  }

  private def execute(cmd: String): TaskResult = ctx.host match {
    case Localhost => executeLocal(cmd)
    case remoteHost: Host => executeRemoteSsh(remoteHost, cmd)
  }
}
