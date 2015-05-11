package net.codejitsu.saruman.dsl

import java.io.File
import java.util.UUID
import org.scalatest.{FlatSpec, Matchers}

/**
 * RmTask tests.
 */
class RmTest extends FlatSpec with Matchers {
  import scala.concurrent.duration._
  import Dsl._

  implicit val timeout = 30 seconds

  "Rm task" should "remove a file with given name on given host" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)

    file2create.exists should be (false)

    val touchTask: Task = Touch(Localhost, path + name)

    val touchResult = touchTask.run

    touchResult.isSuccess should be (true)
    touchResult.get.success should be (true)
    touchResult.get.out should be (empty)
    touchResult.get.err should be (empty)

    file2create.exists should be (true)

    val rmTask: Task = Rm(Localhost, path + name)

    val rmResult = rmTask.run

    rmResult.isSuccess should be (true)
    rmResult.get.success should be (true)
    rmResult.get.out should be (empty)
    rmResult.get.err should be (empty)

    file2create.exists should be (false)
  }

  it should "compose with the touch task" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)

    file2create.exists should be (false)

    val task = for {
      tr <- Touch(Localhost, path + name)
      rr <- Rm(Localhost, path + name)
    } yield rr

    val result = task.run

    result.isSuccess should be (true)
    result.get.success should be (true)
    result.get.out should be (empty)
    result.get.err should be (empty)

    file2create.exists should be (false)
  }

  it should "compose with the touch task with `andThen`" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)

    file2create.exists should be (false)

    val task =
      Touch(Localhost, path + name) andThen
      Rm(Localhost, path + name)

    val result = task.run

    result.isSuccess should be (true)
    result.get.success should be (true)
    result.get.out should be (empty)
    result.get.err should be (empty)

    file2create.exists should be (false)
  }

  it should "return error if file don't exists" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)

    file2create.exists should be (false)

    val task = for {
      tr  <- Touch(Localhost, path + name)
      rr1 <- Rm(Localhost, path + name)
      rr2 <- Rm(Localhost, path + name)
    } yield rr2

    val result = task.run

    result.isSuccess should be (false)
    file2create.exists should be (false)
  }
}
