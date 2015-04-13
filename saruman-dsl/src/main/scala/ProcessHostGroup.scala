// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

case class ProcessHostGroup(hosts: collection.immutable.Seq[ProcessHost]) {
  def | (ctx: ProcessHost): ProcessHostGroup = ctx match {
    case ValidProcessHost(h) =>
    case InvalidProcessHost(_, orig) =>
  }
}
