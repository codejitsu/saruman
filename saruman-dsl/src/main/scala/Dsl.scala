// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

/**
 * DSL for saruman scripting.
 */
object Dsl {
  implicit class HostStringOps(val ctx: String) {
    def ~ (part: String): Host = Host(List(HostPart(ctx), HostPart(part)))

    def ~[P](parts: IndexedSeq[P]): Hosts = {
      val all = for {
        y <- parts
      } yield Host(List(HostPart(ctx), HostPart(y.toString)))

      Hosts(all.toList)
    }

    def ~(parts: Product): Hosts = {
      val flatProduct = for {
        i <- 0 until parts.productArity
      } yield parts.productElement(i).toString

      this ~ flatProduct
    }

    def on (ps: ProcessStep): Process = Process(ctx, ps.host, ps.proc)

    def on (ps: ProcessSteps): Processes = {
      val p = ps.steps map (s => ctx on s)
      Processes(p)
    }

    def ~>(proc: PartialFunction[Command, CommandLine]): ProcessStep = Host(List(HostPart(ctx))) ~> proc
  }

  implicit class HostRangeOps[T](val ctx: IndexedSeq[T]) {
    def ~ (part: String): Hosts = {
      val mapped: collection.immutable.Seq[Host] =
        ctx.map(p => Host(List(HostPart(p.toString), HostPart(part)))).toVector
      Hosts(mapped)
    }

    def ~[P](parts: IndexedSeq[P]): Hosts = {
      val all = for {
        x <- ctx
        y <- parts
      } yield Host(List(HostPart(x.toString), HostPart(y.toString)))

      Hosts(all.toList)
    }

    def ~(parts: Product): Hosts = {
      val flatProduct = for {
        i <- 0 until parts.productArity
      } yield parts.productElement(i).toString

      this ~ flatProduct
    }
  }

  implicit class HostProductOps(val ctx: Product) {
    def ~ (part: String): Hosts = {
      val vals = for {
        i <- 0 until ctx.productArity
      } yield ctx.productElement(i).toString

      val mapped: collection.immutable.Seq[Host] =
        vals.map(p => Host(List(HostPart(p), HostPart(part)))).toVector
      Hosts(mapped)
    }

    def ~[T](parts: IndexedSeq[T]): Hosts = {
      val vals = for {
        i <- 0 until ctx.productArity
      } yield ctx.productElement(i).toString

      val all = for {
        x <- vals
        y <- parts
      } yield Host(List(HostPart(x.toString), HostPart(y.toString)))

      Hosts(all.toList)
    }
  }

  implicit class HostOps(val ctx: Host) {
    def ~>(proc: PartialFunction[Command, CommandLine]): ProcessStep = ProcessStep(proc, host = ctx)
  }

  implicit class HostsOps(val ctx: Hosts) {
    def ~>(proc: PartialFunction[Command, CommandLine]): ProcessSteps = {
      val steps = ctx.hosts map (h => h ~> proc)
      ProcessSteps(steps)
    }
  }

  implicit class ProcessOps(val ctx: Process) {
    def ! (op: Command)(implicit user: User): Task = new ShellTask(ctx, op)
  }

  implicit class ProcessesOps(val ctx: Processes) {
    import scala.concurrent.duration._

    def ! (op: Command)(implicit user: User): TaskM[TaskResult] = {
      val tasks = ctx.procs.map(_ ! op)

      tasks.foldLeft[TaskM[TaskResult]](EmptyTask)((acc, t) => acc flatMap(_ => t))
    }

    def !! (op: Command)(implicit user: User, timeout: Duration): Task = {
      val tasksF = ctx.procs
        .map(_ ! op)
        .map(t => () => Future {
          t.run
        })

      new Task {
        override def run: Try[TaskResult] = Try {
          val tasksFRes = Future.sequence(tasksF.map(_()))

          val result = Await.result(tasksFRes, timeout)

          val resultSuccess = result.map(_.isSuccess).forall(identity)

          val resultOut = result.
            filter(_.isSuccess).
            map(_.get.out).
            foldLeft(List.empty[String])((acc, out) => acc ++ out)

          val resultErr = result.
            filter(_.isSuccess).
            map(_.get.err).
            foldLeft(List.empty[String])((acc, err) => acc ++ err)

          TaskResult(resultSuccess, resultOut, resultErr)
        }
      }
    }
  }

  object Sudo {
    def ~ (exec: Exec): SudoExec = SudoExec(exec.path, exec.params :_*)
  }

  implicit def host2Hosts(host: Host): Hosts = Hosts(List(host))
}
