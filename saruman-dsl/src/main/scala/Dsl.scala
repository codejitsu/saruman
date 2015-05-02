// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

import scala.concurrent.Future

/**
 * DSL for saruman scripting.
 */
object Dsl {
  implicit class HostStringOps(val ctx: String) {
    def ~ (part: String): Host = Host(List(HostPart(ctx), HostPart(part)))

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
    def ! (op: Command)(implicit user: User): Task = {
      val tasks = ctx.procs map { p =>
        p ! op
      }

      tasks.foldLeft[Task](EmptyTask)((acc, t) => acc flatMap(_ => t))
    }
  /*
    def !! (op: Command)(implicit user: User): Task = {
      val tasks = ctx.procs map { p =>
        p ! op
      }

      val tasksF = tasks map { task =>
        new Task {
          override def run: Future[TaskResult] = Future {
            task()
          }
        }
      }
    }
    */
  }

  object Sudo {
    def ~ (exec: Exec): SudoExec = SudoExec(exec.path, exec.params :_*)
  }
}
