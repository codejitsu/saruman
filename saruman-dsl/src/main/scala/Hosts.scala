// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * Group of hosts.
 */
case class Hosts(hosts: collection.immutable.Seq[Host]) {
  def ~ (part: String): Hosts = {
    val appended = hosts.map(h => Host(h.parts :+ HostPart(part)))

    Hosts(appended)
  }
}
