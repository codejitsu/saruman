package net.codejitsu.saruman.dsl

import java.io.File

import org.scalatest.{Matchers, FlatSpec}

/**
 * TouchTask tests.
 */
class TouchTest extends FlatSpec with Matchers {
  import scala.concurrent.duration._

  implicit val timeout = 30 seconds

  "Touch task" should "create a file with given name on given host" in {
    implicit val user = LocalUser("me")

    val path = getClass.getResource("/program-param.sh").getPath.split("/").init.mkString("/") + "/"
    val file2create = new File(path + "testfile.txt")

    file2create.exists should be (false)

    val touchTask: Task = Touch(Hosts(List(Localhost)))(path + "testfile.txt")

    val touchResult = touchTask.run

    touchResult.success should be (true)
    touchResult.out should be (empty)
    touchResult.err should be (empty)

    file2create.exists should be (true)

    file2create.delete
  }
}