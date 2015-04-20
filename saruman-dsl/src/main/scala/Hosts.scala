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

  def ~ (parts: Product): Hosts = {
    val vals = for {
      i <- 0 until parts.productArity
    } yield parts.productElement(i).toString

    val all = vals map (v => this ~ v)

    val together = all.foldLeft(collection.immutable.Seq.empty[Host])((hseq, hosts) => hosts.hosts ++ hseq)

    Hosts(together)
  }
}
