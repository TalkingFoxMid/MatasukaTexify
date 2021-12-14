package ru.talkingfox.matasuka.wolfram
import cats.{Applicative, Functor, Id}
import cats.syntax.all._
import mouse.all._
import ru.talkingfox.matasuka.math.{IntegralDefinite, IntegralIndefinite, MathExpression}
import ru.talkingfox.matasuka.tex.TexInterpreter

class WolframSolver[F[_]: Functor](implicit wfs: WolframScript[F], wfi: WolframInterpreter, texInterpreter: TexInterpreter) {
  def solveIndefinite[A <: MathExpression](indefinite: IntegralIndefinite[A]): F[MathExpression] =
    solveAbstract(indefinite)

  def solveDefinite[A <: MathExpression, B <: MathExpression, C <: MathExpression](definite: IntegralDefinite[A, B, C]): F[MathExpression] =
    solveAbstract(definite)

  private def solveAbstract(me: MathExpression): F[MathExpression] = {
    val interpreted = wfi.interprete(me)
    println(interpreted)
    val solved = wfs.solveTeX(s"Rationalize[${interpreted}]")

    val expr = solved.map(texInterpreter.untexify)
    expr
  }
}
