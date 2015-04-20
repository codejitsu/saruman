package net.codejitsu.saruman.dsl

import org.scalacheck.{Arbitrary, Prop, Gen, Properties}

/**
 * Test for process host dsl.
 */
class ProcessHostDslTest extends Properties("ProcessHostDsl") {
  import Prop.forAll
  import Arbitrary.arbitrary
  import Dsl._

/*
  def genMachine(id: String, subnet: List[Int]): Gen[Machine] = for {
    uuid <- Gen.uuid
    ip <- Gen.choose(2,254).map(n => s"172.16.2.$n")
    memory <- Gen.choose(96, 256)
    kernel <- Gen.oneOf("3.14", "3.13", "3.12", "3.10")
  } yield Machine (id, uuid, ip, kernel, memory, false)
*/

  def genPart: Gen[String] = for {
    id <- Gen.identifier
    str <- arbitrary[String]
    together <- Gen.oneOf(id, str)
  } yield together

  val genSimpleHostPart: Gen[String] = for {
    partsCount <- Gen.choose(0, 10)
    partGen = genPart
    ids <- Gen.listOfN(partsCount, partGen)
    //tld <- Gen.oneOf("com", "net", "org", "de", "eu", "am", "la", "ua", "club")
  } yield ids.mkString(".")

  val generateHostWithRange: Gen[(String, Range)] = for {
    rangeMin <- Gen.choose(0, 10)
    rangeMax <- Gen.choose(11, 15)
    range = rangeMin to rangeMax
    host <- genSimpleHostPart
  } yield (host, range)
/*
  val genTuple: Gen[Product] = for {
    size <- Gen.choose(1, 22)
  }
*/
  property("For all valid simple host name part generates a valid ProcessHost with specified name") = forAll(genSimpleHostPart) { hostPart =>
    val host = hostPart.h

    host match {
      case v @ ValidProcessHost(_) => v.name == hostPart
      case InvalidProcessHost(_, _) => true
    }
  }

  property("Using ranges generates valid sequence of all hosts") = forAll(generateHostWithRange) { h =>
    val host = h._1.h

    host match {
      case v @ ValidProcessHost(parts) =>
        val hosts = v | h._2

        h._2.forall(i => hosts.hosts.exists(host => host.name.endsWith(i.toString))) && v.name == h._1

      case InvalidProcessHost(_, _) => true
    }
  }
/*
  property("For all strings, ranges and tuples dsl will generate a valide hosts") = forAll {

  }
  */
}
