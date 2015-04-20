// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * DSL for saruman scripting.
 */
object Dsl {
  implicit class ProcessHostForStringContext(val ctx: String) {
    def h: ProcessHost = {
      val parts = for {
        part <- ctx.split('.')
      } yield part

      val allPartsValid = isInputValid(parts)

      if (allPartsValid) {
        ValidProcessHost(parts.toList)
      } else {
        val firstInvalid = for {
          part <- parts
          if !isPartValid(part)
        } yield part

        InvalidProcessHost(firstInvalid.headOption.getOrElse(""), ctx)
      }
    }
/*
    def | (r: Range): ProcessHostGroup = {
      val host = ctx h

      host | r
    }
*/
    private def isInputValid(input: Array[String]): Boolean =
      ctx.trim.length > 0 &&
      ctx.last.isLetterOrDigit &&
      !input.isEmpty && input.forall(isPartValid)

    private def isPartValid(value: String): Boolean = {
      val res = Option(value).isDefined &&
        value.trim.length > 0 &&
        value.trim.forall(ch => ch.isDigit || ('a' to 'z').contains(ch.toLower))

      res
    }
  }

  implicit class HostStringOps(val ctx: String) {
    def ~ (part: String): Host = Host(List(HostPart(ctx), HostPart(part)))
  }

  implicit class HostRangeOps[T](val ctx: IndexedSeq[T]) {
    def ~ (part: String): Hosts = {
      val mapped: collection.immutable.Seq[Host] =
        ctx.map(p => Host(List(HostPart(p.toString), HostPart(part)))).toVector
      Hosts(mapped)
    }

    def ~[P](parts: IndexedSeq[P]): Hosts = {
      val all = for {
        x <- ctx
        y <- parts
      } yield Host(List(HostPart(x.toString), HostPart(y.toString)))

      Hosts(all.toList)
    }

    def ~(parts: Product): Hosts = {
      val flatProduct = for {
        i <- 0 until parts.productArity
      } yield parts.productElement(i).toString

      this ~ flatProduct
    }
  }

  implicit class HostProductOps(val ctx: Product) {
    def ~ (part: String): Hosts = {
      val vals = for {
        i <- 0 until ctx.productArity
      } yield ctx.productElement(i).toString

      val mapped: collection.immutable.Seq[Host] =
        vals.map(p => Host(List(HostPart(p), HostPart(part)))).toVector
      Hosts(mapped)
    }

    def ~[T](parts: IndexedSeq[T]): Hosts = {
      val vals = for {
        i <- 0 until ctx.productArity
      } yield ctx.productElement(i).toString

      val all = for {
        x <- vals
        y <- parts
      } yield Host(List(HostPart(x.toString), HostPart(y.toString)))

      Hosts(all.toList)
    }
  }
}
