package net.codejitsu.saruman.dsl

import org.scalatest.{Matchers, FlatSpec}

/**
 * DSL tests.
 */
class DslTest extends FlatSpec with Matchers {
  import Dsl._

  "DSL" should "allow to define simple hosts" in {
    val host = "my.host.name".h

    host.name should be ("my.host.name")
  }

  it should "allow to define hosts with ranges (1)" in {
    val group = "my.host.name.part" | (1 to 5) | ".com".h

    val names = group.hosts.map(_.name)

    val expected = List(
      "my.host.name.part1.com",
      "my.host.name.part2.com",
      "my.host.name.part3.com",
      "my.host.name.part4.com",
      "my.host.name.part5.com"
    )

    group.hosts.size should be (5)
    expected.forall(name => names.exists(h => h == name))
  }

  it should "allow to define hosts with ranges (2)" in {
    val group = "my.host.name.part." | (1 to 5) | ".com".h

    val names = group.hosts.map(_.name)

    val expected = List(
      "my.host.name.part.1.com",
      "my.host.name.part.2.com",
      "my.host.name.part.3.com",
      "my.host.name.part.4.com",
      "my.host.name.part.5.com"
    )

    group.hosts.size should be (5)
    expected.forall(name => names.exists(h => h == name))
  }

  it should "allow to define hosts with ranges (3)" in {
    val group = "my.host.name.part" | (1 to 5) | "com".h

    val names = group.hosts.map(_.name)

    val expected = List(
      "my.host.name.part1com",
      "my.host.name.part2com",
      "my.host.name.part3com",
      "my.host.name.part4com",
      "my.host.name.part5com"
    )

    group.hosts.size should be (5)
    expected.forall(name => names.exists(h => h == name))
  }
}
