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

    def | (r: Range): ProcessHostGroup = {
      val host = ctx h

      host | r
    }

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
}
