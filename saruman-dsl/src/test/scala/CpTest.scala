package net.codejitsu.saruman.dsl

import java.io.File
import java.util.UUID
import org.scalatest.{FlatSpec, Matchers}

/**
 * Cp tests.
 */
class CpTest extends FlatSpec with Matchers {
  import scala.concurrent.duration._
  import Dsl._

  implicit val timeout = 30 seconds

  "Cp task" should "copy a file with given name on given host" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val namecopy = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)
    val file2copy = new File(path + namecopy)

    file2create.exists should be (false)
    file2copy.exists should be (false)

    val touchTask: Task = Touch(Localhost, path + name)

    val touchResult = touchTask.run

    touchResult.isSuccess should be (true)
    touchResult.get.success should be (true)
    touchResult.get.out should be (empty)
    touchResult.get.err should be (empty)

    file2create.exists should be (true)

    val copyTask: Task = Cp(Localhost, path + name, path + namecopy)

    val copyResult = copyTask.run

    copyResult.isSuccess should be (true)
    copyResult.get.success should be (true)
    copyResult.get.out should be (empty)
    copyResult.get.err should be (empty)

    file2copy.exists should be (true)
  }

  it should "compose with the touch task" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val namecopy = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)
    val file2copy = new File(path + namecopy)

    file2create.exists should be (false)
    file2copy.exists should be (false)

    val task = for {
      tr <- Touch(Localhost, path + name)
      cr <- Cp(Localhost, path + name, path + namecopy)
    } yield cr

    val result = task.run

    result.isSuccess should be (true)
    result.get.success should be (true)
    result.get.out should be (empty)
    result.get.err should be (empty)

    file2copy.exists should be (true)
  }

  it should "compose with the touch task with `andThen`" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val namecopy = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)
    val file2copy = new File(path + namecopy)

    file2create.exists should be (false)
    file2copy.exists should be (false)

    val task =
      Touch(Localhost, path + name) andThen
      Cp(Localhost, path + name, path + namecopy)

    val result = task.run

    result.isSuccess should be (true)
    result.get.success should be (true)
    result.get.out should be (empty)
    result.get.err should be (empty)

    file2copy.exists should be (true)
  }

  it should "return error if file don't exists" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val name = s"${UUID.randomUUID().toString}testfile.txt"
    val namecopy = s"${UUID.randomUUID().toString}testfile.txt"
    val file2create = new File(path + name)
    val file2copy = new File(path + namecopy)

    file2create.exists should be (false)
    file2copy.exists should be (false)

    val task = for {
      tr  <- Touch(Localhost, path + name)
      cp <- Cp(Localhost, path + name + "1", path + namecopy)
    } yield cp

    val result = task.run

    result.isSuccess should be (false)

    file2copy.exists should be (false)
  }
}
