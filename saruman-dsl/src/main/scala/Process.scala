// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

sealed trait ProcessOps

case object Start extends ProcessOps
case object Stop extends ProcessOps

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

case class Process(name: String, hosts: collection.immutable.Seq[Host], proc: PartialFunction[ProcessOps, ProcessCmd]) {
  def startCmd: ProcessCmd = {
    if (proc.isDefinedAt(Start)) proc(Start)
    else NoExec
  }

  def stopCmd: ProcessCmd = {
    if (proc.isDefinedAt(Stop)) proc(Stop)
    else NoExec
  }
}

case class ProcessStep(proc: PartialFunction[ProcessOps, ProcessCmd],
                       name: String = "", hosts: collection.immutable.Seq[Host] = Nil)
