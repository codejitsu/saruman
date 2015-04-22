// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Task.
 */
case class TaskResult(success: Boolean, out: String)

trait Task {
  def process: Process
  def cmd: ProcessTask

  def run: TaskResult
}
