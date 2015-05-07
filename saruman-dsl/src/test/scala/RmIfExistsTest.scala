package net.codejitsu.saruman.dsl

import java.io.File
import org.scalatest.{FlatSpec, Matchers}

/**
 * RmIfExists tests.
 */
class RmIfExistsTest extends FlatSpec with Matchers {
  import scala.concurrent.duration._
  import Dsl._

  implicit val timeout = 30 seconds

  "RmIfExists task" should "remove a file with given name on given host" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val file2create = new File(path + "testfile.txt")

    file2create.exists should be (false)

    val touchTask: Task = Touch(Localhost, path + "testfile.txt")

    val touchResult = touchTask.run

    touchResult.success should be (true)
    touchResult.out should be (empty)
    touchResult.err should be (empty)

    file2create.exists should be (true)

    val rmTask: Task = RmIfExists(Localhost, path + "testfile.txt")

    val rmResult = rmTask.run

    println(rmResult)

    rmResult.success should be (true)
    rmResult.out should be (empty)
    rmResult.err should be (empty)

    file2create.exists should be (false)
  }

  it should "compose with the touch task" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val file2create = new File(path + "testfile.txt")

    file2create.exists should be (false)

    val task = for {
      tr <- Touch(Localhost, path + "testfile.txt")
      rr <- RmIfExists(Localhost, path + "testfile.txt")
    } yield rr

    val result = task.run

    result.success should be (true)
    result.out should be (empty)
    result.err should be (empty)

    file2create.exists should be (false)
  }

  it should "compose with the touch task with `andThen`" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val file2create = new File(path + "testfile.txt")

    file2create.exists should be (false)

    val task =
      Touch(Localhost, path + "testfile.txt") andThen
        RmIfExists(Localhost, path + "testfile.txt")

    val result = task.run

    result.success should be (true)
    result.out should be (empty)
    result.err should be (empty)

    file2create.exists should be (false)
  }

  it should "not return error if file dont exists" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val file2create = new File(path + "testfile.txt")

    file2create.exists should be (false)

    val task = for {
      tr  <- Touch(Localhost, path + "testfile.txt")
      rr1 <- RmIfExists(Localhost, path + "testfile.txt")
      rr2 <- RmIfExists(Localhost, path + "testfile.txt")
    } yield rr2

    val result = task.run

    result.success should be (true)
    result.out should be (empty)
    result.err should be (empty)

    file2create.exists should be (false)
  }
}
