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
/*
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
    val group = "my.host.name.part" :: (1 to 5) :: "com".h

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
  */

  it should "allow to compose two host parts together with ~" in {
    val host: Host = "my" ~ "test" ~ "host" ~ "system.1"

    host.toString should be ("my.test.host.system.1")
  }

  it should "allow to compose strings and ranges with ~" in {
    val hosts: Hosts = "my" ~ "test" ~ "host" ~ (1 to 3) ~ "system.1"

    val all = hosts.hosts map (_.toString())

    all should be (List("my.test.host.1.system.1", "my.test.host.2.system.1", "my.test.host.3.system.1"))
  }

  it should "allow to compose ranges and strings with ~" in {
    val hosts: Hosts = (1 to 3) ~ "system.1" ~ "my" ~ "test" ~ "host"

    val all = hosts.hosts map (_.toString())

    all should be (List("1.system.1.my.test.host", "2.system.1.my.test.host", "3.system.1.my.test.host"))
  }

  it should "allow to compose ranges and ranges with ~" in {
    val hosts: Hosts = (1 to 3) ~ ('a' to 'b') ~ "system.1" ~ "my" ~ "test" ~ "host"

    val all = hosts.hosts map (_.toString())

    all should be (List("1.a.system.1.my.test.host", "1.b.system.1.my.test.host", "2.a.system.1.my.test.host",
      "2.b.system.1.my.test.host", "3.a.system.1.my.test.host", "3.b.system.1.my.test.host"))
  }

  it should "allow to compose ranges and tuples with ~" in {
    val hosts: Hosts = (1 to 3) ~ (('a', 'b')) ~ "system.1" ~ "my" ~ "test" ~ "host"

    val all = hosts.hosts map (_.toString())

    all should be (List("1.a.system.1.my.test.host", "1.b.system.1.my.test.host", "2.a.system.1.my.test.host",
      "2.b.system.1.my.test.host", "3.a.system.1.my.test.host", "3.b.system.1.my.test.host"))
  }

  it should "allow to compose tuples and ranges with ~" in {
    val hosts: Hosts = ((1, 2, 3)) ~ ('a' to 'b') ~ "system.1" ~ "my" ~ "test" ~ "host"

    val all = hosts.hosts map (_.toString())

    all should be (List("1.a.system.1.my.test.host", "1.b.system.1.my.test.host", "2.a.system.1.my.test.host",
      "2.b.system.1.my.test.host", "3.a.system.1.my.test.host", "3.b.system.1.my.test.host"))
  }

  it should "allow to compose strings and tuples with ~" in {
    val hosts: Hosts = "my" ~ "test" ~ "host" ~ (("abc", 100, 'z')) ~ "system.1"

    val all = hosts.hosts map (_.toString())

    all should be (List("my.test.host.abc.system.1", "my.test.host.100.system.1", "my.test.host.z.system.1"))
  }

  it should "allow to compose tuples and tuples with ~" in {
    val hosts: Hosts = "my" ~ "test" ~ "host" ~ (("abc", 100, 'z')) ~ (("one", "two")) ~ "system.1"

    val all = hosts.hosts map (_.toString())

    all.sorted should be (List("my.test.host.abc.one.system.1", "my.test.host.abc.two.system.1", "my.test.host.100.one.system.1",
      "my.test.host.100.two.system.1", "my.test.host.z.one.system.1", "my.test.host.z.two.system.1").sorted)
  }

  it should "allow to compose tuples and strings with ~" in {
    val hosts: Hosts = (("abc", 100, 'z')) ~ "my" ~ "test" ~ "host" ~ "system.1"

    val all = hosts.hosts map (_.toString())

    all should be (List("abc.my.test.host.system.1", "100.my.test.host.system.1", "z.my.test.host.system.1"))
  }
}
