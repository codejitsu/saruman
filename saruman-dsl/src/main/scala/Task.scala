// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Task.
 */
case class TaskResult(success: Boolean, out: List[String])

trait Task {
  self =>

  def process: Process
  def cmd: ProcessTask

  def run: TaskResult

  def andThen(task: Task): Task = new Task {
    override def run: TaskResult = {
      val thisResult = self.run
      val taskResult = task.run

      TaskResult(thisResult.success && taskResult.success, thisResult.out ++ taskResult.out)
    }

    override def process: Process = ??? //TODO ComposedProcess

    override def cmd: ProcessTask = ComposedTask
  }
}
