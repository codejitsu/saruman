// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

sealed trait ProcessOps

case object Start extends ProcessOps
case object Stop extends ProcessOps

class Process(hosts: collection.immutable.Seq[Host], proc: PartialFunction[ProcessOps, String])

case class ProcessStep(hosts: collection.immutable.Seq[Host]) {
  def to (proc: PartialFunction[ProcessOps, String]): Process = new Process(hosts, proc)
}
