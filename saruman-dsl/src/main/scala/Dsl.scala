// Copyright (C) 2015, codejitsu.

package net.codejitsu.saruman.dsl

/**
 * DSL for saruman scripting.
 */
object Dsl {
  implicit class ProcessHostForStringContext(val ctx: StringContext) {
    def h(values: Any*): ProcessHost = {
      val parts = for {
        v <- values
        parts = v.toString
        part <- parts.split('.')
      } yield part

      val allPartsValid = parts.forall(isPartValid)

      if (allPartsValid) {
        ValidProcessHost(values.map(_.toString): _*)
      } else {
        val firstInvalid = for {
          part <- parts
          if !isPartValid(part)
        } yield part

        new InvalidProcessHost(firstInvalid.headOption.getOrElse(""))
      }
    }

    private def isPartValid(value: String): Boolean = {
      val res = Option(value).isDefined &&
        value.trim.length > 0 &&
        value.trim.forall(ch => ch.isDigit || ('a' to 'z').contains(ch.toLower))

      res
    }
  }
}
