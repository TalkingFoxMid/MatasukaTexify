package ru.talkingfox.matasuka.math.math_syntax

import ru.talkingfox.matasuka.markdown.Markdown
import ru.talkingfox.matasuka.math.{MathExpression, Mathable, Statement, Term}
import ru.talkingfox.matasuka.math.Statements._
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.EquationSolver

trait StatementSyntax {
  implicit def statementMathable[A <: Statement]: Mathable[A, A] = identity
  implicit def stringMathable: Mathable[String, Term] =
    new Mathable[String, Term] {
      override def mathify(a: String): Term = Term(a)
    }

  implicit class EqvOps(eqv: Eqv) {
    def solveEqv[F[_]](term: Term)(implicit equationSolver: EquationSolver[F]):
      F[Map[Term, MathExpression]] =
      equationSolver.solveEqv(eqv, term)
  }

  implicit class StatementOps[I :Mathable[*, A], A <: Statement](a: I) {
    val stmt: A = Mathable[I, A].mathify(a)

    def md(implicit interpreter: TexInterpreter): Markdown[A] = new Markdown[A](stmt, s"$$${interpreter.texifyStatement(stmt)}$$" :: Nil)

    def eqv[O: Mathable[*, Statement]](other: O): Eqv = Eqv(stmt, Mathable[O, Statement].mathify(other))

    def ===[O: Mathable[*, Statement]](other: O): Eqv = Eqv(stmt, Mathable[O, Statement].mathify(other))

    def neq[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), "\\neq")

    def <<<[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), "<")

    def >>>[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), ">")

    def <=<[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), "\\leq")

    def >=>[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), "\\geq")

    def ==>[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), "\\implies")

    def comma[O: Mathable[*, Statement]](other: O): Relation =
      Relation(stmt, Mathable[O, Statement].mathify(other), ",\\ ")

    def norm[O: Mathable[*, Statement]]: Norm =
      Norm(stmt)

    def asTerm: Term =
      Term(stmt.toString)
  }
}
