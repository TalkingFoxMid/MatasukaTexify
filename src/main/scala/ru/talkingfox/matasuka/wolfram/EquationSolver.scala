package ru.talkingfox.matasuka.wolfram

import cats.Functor
import cats.syntax.all._
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka.math.Statements.Eqv
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.syntax._

class EquationSolver[F[_]: Functor](implicit wolframScript: WolframScript[F],
                                    texInterpreter: TexInterpreter) {
  def solveEqv(equation: Eqv, term: Term): F[Map[Term, MathExpression]] = {
    equation match {
      case Eqv(a: MathExpression, b: MathExpression) =>
        wolframScript.solveTeX {
          s"Solve[${a.wolf} == ${b.wolf}, ${term.wolf}]"
        }.map {
          _.replace("\\{", "")
            .replace("\\}", "")
            .split(",")
            .toList
            .map(
              _.split("\\\\to") match {
                case Array(a, b) => (Term(a), texInterpreter.untexify(b))
              }
            ).toMap
        }
      case _ => throw new RuntimeException("Cannot!")
    }
  }
}
