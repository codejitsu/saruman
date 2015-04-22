// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Task.
 */
case class TaskResult()

trait Task {
  def process: Process
  def cmd: ProcessTask

  def run: TaskResult
}
