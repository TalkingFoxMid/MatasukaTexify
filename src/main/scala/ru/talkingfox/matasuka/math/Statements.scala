package ru.talkingfox.matasuka.math

import ru.talkingfox.matasuka.markdown.{Markdown, Markdownable}
import ru.talkingfox.matasuka.tex.TexInterpreter

trait Statement

object Statements {
  case class Eqv(a: Statement, b: Statement) extends Statement

  case class Relation(a: Statement, b: Statement, char: String) extends Statement

  case class Norm(a: Statement) extends Statement

  case class Cases(css: List[Statement]) extends Statement {
    def addCase(stmt: Statement): Cases = Cases(stmt :: css)
  }

  object Cases {
    def apply(): Cases = Cases(Nil)
  }
}
