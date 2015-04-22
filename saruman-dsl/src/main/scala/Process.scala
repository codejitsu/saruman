// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

sealed trait ProcessTask {
  def cmd: String
}

case object Start extends ProcessTask {
  val cmd = "start"
}

case object Stop extends ProcessTask {
  val cmd = "stop"
}

case object ComposedTask extends ProcessTask {
  val cmd = "composed"
}

sealed trait ProcessCmd {
  def path: String
  def args: Array[String]
  def cmd: String = s"$path ${args.mkString(" ")}"
}

case class Exec(path: String, params: String*) extends ProcessCmd {
  def args: Array[String] = params.toArray
}

case class SudoExec(path: String, params: String*) extends ProcessCmd {
  def args: Array[String] = params.toArray
  override def cmd: String = s"sudo ${super.cmd}"
}

case object NoExec extends ProcessCmd {
  override val path: String = ""
  override val args: Array[String] = Array.empty[String]
}

case class Process(name: String, host: Host, proc: PartialFunction[ProcessTask, ProcessCmd], verbose: Boolean = false) {
  def startCmd: ProcessCmd = {
    if (proc.isDefinedAt(Start)) {
      proc(Start)
    } else {
      NoExec
    }
  }

  def stopCmd: ProcessCmd = {
    if (proc.isDefinedAt(Stop)) {
      proc(Stop)
    } else {
      NoExec
    }
  }
}

case class ProcessStep(proc: PartialFunction[ProcessTask, ProcessCmd], host: Host)

case class ProcessSteps(steps: collection.immutable.Seq[ProcessStep])

case class Processes(procs: collection.immutable.Seq[Process])
