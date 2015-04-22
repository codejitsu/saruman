// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

sealed trait ProcessTask {
  def cmd: String
}

case object Start extends ProcessTask {
  val cmd = "start"  
}

case object Restart extends ProcessTask {
  val cmd = "restart"
}

case object Stop extends ProcessTask {
  val cmd = "stop"
}

sealed trait ProcessCmd {
  def path: String
  def args: Array[String]
}

case class Exec(path: String, params: String*) extends ProcessCmd {
  def args: Array[String] = params.toArray
}

case object NoExec extends ProcessCmd {
  override val path: String = ""
  override val args: Array[String] = Array.empty[String]
}

case class Process(name: String, host: Host, proc: PartialFunction[ProcessTask, ProcessCmd]) {
  def startCmd: ProcessCmd = {
    if (proc.isDefinedAt(Start)) proc(Start)
    else NoExec
  }

  def stopCmd: ProcessCmd = {
    if (proc.isDefinedAt(Stop)) proc(Stop)
    else NoExec
  }
}

case class ProcessStep(proc: PartialFunction[ProcessTask, ProcessCmd], host: Host)

case class ProcessSteps(steps: collection.immutable.Seq[ProcessStep])

case class Processes(procs: collection.immutable.Seq[Process])