package net.codejitsu.saruman.dsl

import org.scalatest.{Matchers, FlatSpec}

/**
 * DSL tests.
 */
class DslTest extends FlatSpec with Matchers {
  import Dsl._

  "DSL" should "allow to compose two host parts together with ~" in {
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

  it should "allow to define process on host" in {
    val host = "my" ~ "test" ~ "host"

    val process: Process = "tomcat" on host ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    process.name should be ("tomcat")
    process.startCmd should be (Exec("/etc/init.d/tomcat", "start"))
    process.stopCmd should be (Exec("/etc/init.d/tomcat", "stop"))
    process.host should be (host)
  }

  it should "allow to define process on simple host" in {
    val process: Process = "tomcat" on "my.test.host" ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    process.name should be ("tomcat")
    process.startCmd should be (Exec("/etc/init.d/tomcat", "start"))
    process.stopCmd should be (Exec("/etc/init.d/tomcat", "stop"))
    process.host should be (Host(List(HostPart("my.test.host"))))
  }

  it should "allow to define process on localhost" in {
    val process: Process = "tomcat" on Localhost ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    process.name should be ("tomcat")
    process.startCmd should be (Exec("/etc/init.d/tomcat", "start"))
    process.stopCmd should be (Exec("/etc/init.d/tomcat", "stop"))
    process.host should be (Localhost)
  }

  it should "allow to define processes on multiple hosts" in {
    val hosts: Hosts = "my" ~ "test" ~ "host" ~ (1 to 10)

    val tomcats: Processes = "tomcat" on hosts ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    tomcats.procs.size should be (10)

    tomcats.procs.map(_.host.toString).toSet.size should be (10)

    tomcats.procs.foreach { tomcat =>
      tomcat.name should be ("tomcat")
      tomcat.startCmd should be (Exec("/etc/init.d/tomcat", "start"))
      tomcat.stopCmd should be (Exec("/etc/init.d/tomcat", "stop"))
      tomcat.host.toString.startsWith("my.test.host") should be (true)
    }
  }

  it should "allow to define processes on multiple hosts (simple view)" in {
    val hosts = "my" ~ "test" ~ "host" ~ (1 to 10)

    val tomcats = "tomcat" on hosts ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    tomcats.procs.size should be (10)

    tomcats.procs.map(_.host.toString).toSet.size should be (10)

    tomcats.procs.foreach { tomcat =>
      tomcat.name should be ("tomcat")
      tomcat.startCmd should be (Exec("/etc/init.d/tomcat", "start"))
      tomcat.stopCmd should be (Exec("/etc/init.d/tomcat", "stop"))
      tomcat.host.toString.startsWith("my.test.host") should be (true)
    }
  }

  it should "allow to define tasks with processes" in {
    val tomcat: Process = "tomcat" on Localhost ~> {
      case Start => Exec("/etc/init.d/tomcat", "start")
      case Stop => Exec("/etc/init.d/tomcat", "stop")
    }

    val startTomcat: Task = tomcat ! Start
    val stopTomcat: Task = tomcat ! Stop

    assert(Option(startTomcat).isDefined)
    assert(Option(stopTomcat).isDefined)
  }

  it should "run processes on localhost" in {
    implicit val user = LocalUser("me")

    val procStart = "sh " + getClass.getResource("/program-start.sh").getPath
    val procStop = "sh " + getClass.getResource("/program-stop.sh").getPath

    val program: Process = "test" on Localhost ~> {
      case Start => Exec(procStart)
      case Stop => Exec(procStop)
    }

    val startShell: Task = program ! Start
    val startResult = startShell.run

    startResult.success should be (true)
    startResult.out should be (List("start test program"))

    val stopShell: Task = program ! Stop
    val stopResult = stopShell.run

    stopResult.success should be (true)
    stopResult.out should be (List("stop test program"))
  }

  it should "compose processes on localhost" in {
    implicit val user = LocalUser("me")

    val procStart = "sh " + getClass.getResource("/program-start.sh").getPath
    val procStop = "sh " + getClass.getResource("/program-stop.sh").getPath

    val program: Process = "test" on Localhost ~> {
      case Start => Exec(procStart)
      case Stop => Exec(procStop)
    }

    val startShell: Task = program ! Start
    val stopShell: Task = program ! Stop

    val composed = startShell andThen stopShell
    val composedResult = composed.run

    composedResult.success should be (true)
    composedResult.out should be (List("start test program", "stop test program"))
  }

  it should "compose processes on monadic way" in {
    implicit val user = LocalUser("me")

    val procStart = "sh " + getClass.getResource("/program-start.sh").getPath
    val procStop = "sh " + getClass.getResource("/program-stop.sh").getPath

    val program: Process = "test" on Localhost ~> {
      case Start => Exec(procStart)
      case Stop => Exec(procStop)
    }

    val startShell: Task = program ! Start
    val stopShell: Task = program ! Stop

    val composed = for {
      stSh <- startShell
      stopSh <- stopShell
    } yield stopSh

    val composedResult = composed.run

    composedResult.success should be (true)
    composedResult.out should be (List("start test program", "stop test program"))
  }

  it should "compose processes on monadic way (one task)" in {
    implicit val user = LocalUser("me")

    val procStart = "sh " + getClass.getResource("/program-start.sh").getPath
    val procStop = "sh " + getClass.getResource("/program-stop.sh").getPath

    val program: Process = "test" on Localhost ~> {
      case Start => Exec(procStart)
      case Stop => Exec(procStop)
    }

    val startShell: Task = program ! Start
    val stopShell: Task = program ! Stop

    val composed = for {
      stSh <- startShell
    } yield stSh

    val composedResult = composed.run

    composedResult.success should be (true)
    composedResult.out should be (List("start test program"))
  }

  it should "compose processes on monadic way (failure)" in {
    implicit val user = LocalUser("me")

    val procStart = "sh " + getClass.getResource("/program-start.sh").getPath
    val procStop = "sh " + getClass.getResource("/program-stop.sh").getPath

    val program: Process = "test" on Localhost ~> {
      case Start => Exec(procStart)
      case Stop => Exec(procStop)
    }

    val startShellFailure: Task = FailedTask(List(), List("task error"))
    val stopShell: Task = program ! Stop

    val composed = for {
      stSh <- startShellFailure
      stopSh <- stopShell
    } yield stopSh

    val composedResult = composed.run

    composedResult.success should be (false)
    composedResult.out should be (empty)
    composedResult.err should be (List("task error"))
  }
}
