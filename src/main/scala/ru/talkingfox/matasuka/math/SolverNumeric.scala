package ru.talkingfox.matasuka.math
import cats.{Applicative, Monad, MonadError, MonadThrow}
import cats.syntax.all._
import ru.talkingfox.matasuka.wolfram.WolframSolver
import ru.talkingfox.matasuka.math.math_syntax.syntax._

class SolverNumeric[F[_]: MonadThrow](implicit wolframSolver: WolframSolver[F]) {
  def solveNumeric(mathExpression: MathExpression): F[Double] =
    mathExpression match {
      case Frac(m1, m2) => solveNumeric(m1).product(solveNumeric(m2))
        .map {case (a, b) => a / b}
      case Number(a) => a.pure[F]
      case Minus(a) => solveNumeric(a).map(-_)
      case Term(symbol) => throw new RuntimeException("failed")
      case Multiply(exprs) => exprs.traverse(solveNumeric).map(_.product)
      case Plus(exprs) => exprs.traverse(solveNumeric).map(_.sum)
      case Power(m1, m2) => solveNumeric(m1).product(solveNumeric(m2))
        .map {case (a, b) => a * b}
      case Sqrt(target, p) => solveNumeric(target).product(solveNumeric(p))
        .map {case (t, po) => Math.pow(t, 1/po)}
      case Sin(target) => solveNumeric(target).map(Math.sin)
      case Cos(target) => solveNumeric(target).map(Math.cos)
      case i@IntegralIndefinite(_, _) =>
        wolframSolver.solveIndefinite(i).flatMap(solveNumeric)
      case i@IntegralDefinite(_, _, _) =>
        wolframSolver.solveDefinite(i).flatMap(solveNumeric)
      case EConst => Math.E.pure[F]
      case PIConst => Math.PI.pure[F]
      case Function(name, args, a, b) =>
        b.liftTo(new RuntimeException("wrong")).map {
          a.toList.foldLeft(_) {
            case (baza, (term, nexp)) => baza.withExpression(term, nexp)
          }
        }.flatMap(solveNumeric)
      case Sum(body, i, beginV, endV) => {
        for {
          beg <- solveNumeric(beginV)
          end <- solveNumeric(endV)
          lst <- LazyList.iterate(beg)(_ + 1)
            .takeWhile(_ <= end)
            .map(body.withValue(i, _))
            .map(solveNumeric)
            .toList
            .sequence
        } yield lst.sum
      }

    }
}