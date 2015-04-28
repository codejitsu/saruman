// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Task.
 */
case class TaskResult(success: Boolean, out: List[String])

trait Task {
  self =>

  def run: TaskResult

  def andThen(task: Task): Task = new Task {
    override def run: TaskResult = {
      val thisResult = self.run
      val taskResult = task.run

      TaskResult(thisResult.success && taskResult.success, thisResult.out ++ taskResult.out)
    }
  }
}

class RunnableTask(val ctx: Process, val user: User[String], val op: ProcessTask) extends Task {
  import scala.collection.mutable.ListBuffer

  override def run: TaskResult = op match {
    case Start => execute(ctx.startCmd.cmd)

    case Stop => execute(ctx.stopCmd.cmd)

    case _ => ???
  }

  private def executeLocal(cmd: String): TaskResult = {
    import scala.sys.process._

    val out = ListBuffer[String]()

    def doOut(line: String): Unit = {
      out.append(line)

      if(ctx.verbose) {
        println(line)
      }
    }

    if (ctx.verbose) {
      println(s"$op '${ctx.name}' (${cmd.trim}) on '${ctx.host.toString}'")
    }

    val result = (cmd run (ProcessLogger(doOut _))).exitValue

    TaskResult(result == 0, out.toList)
  }

  private def executeRemote(remoteHost: Host, cmd: String): TaskResult = {
    import com.decodified.scalassh._

    val login = PublicKeyLogin(user.username)

    val sshResult = SSH(remoteHost.toString(), login) { sshClient =>
      try sshClient.exec(cmd).right.map { res =>
        TaskResult(true, List(res.stdOutAsString()))
      } finally {
        sshClient.close()
      }
    }

    if (sshResult.isRight) {
      sshResult.right.get
    } else {
      TaskResult(false, List(sshResult.left.get))
    }
  }

  private def execute(cmd: String): TaskResult = ctx.host match {
    case Localhost => executeLocal(cmd)
    case remoteHost: Host => executeRemote(remoteHost, cmd)
  }
}
