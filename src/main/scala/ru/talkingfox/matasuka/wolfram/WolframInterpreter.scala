package ru.talkingfox.matasuka.wolfram

import cats.effect.{ExitCode, IO, IOApp}
import ru.talkingfox.matasuka.wolfram.syntax._
import ru.talkingfox.matasuka.math
import ru.talkingfox.matasuka.math._

class WolframInterpreter {
  implicit val win: WolframInterpreter = this

  def interprete(expr: MathExpression): String = expr match {
    case Frac(m1, m2) => s"${m1.wolf} / ${m2.wolf}"
    case math.Number(a) => a match {
      case i if i % 1 == 0 => s"${a.toInt}"
      case i => s"${i}"
    }
    case Term(symbol) => s"$symbol"
    case Multiply(exp) => exp.map(_.wolf).mkString(" * ")
    case Plus(exp) => exp.map(_.wolf).mkString(" + ")
    case Power(m1, m2) => s"${m1.wolf} ^ ${m2.wolf}"
    case Sqrt(target, p) => s"${target.wolf} ^ (${p.wolf})"
    case Sin(target) => s"Sin[${target.wolf}]"
    case Cos(target) => s"Cos[${target.wolf}]"
    case IntegralIndefinite(core, d) =>
      s"Integrate[${core.wolf}, ${d.wolf}]"
    case IntegralDefinite(IntegralIndefinite(core, d), from, to) =>
      s"Integrate[${core.wolf}, {${d.wolf}, ${from.wolf}, ${to.wolf}}]"
    case Minus(a) => s"-${a.wolf}"
    case EConst => s"E"
    case PIConst => s"Pi"
    case Function(name, args, a, b) => s"${name.wolf}(${args.map(_.wolf).mkString(", ")})"
    case Sum(body, i, beginV, endV) => ???
  }
}
object WolframInterpreter extends IOApp with EndofunctorionIO {

  val res = solver.solveIndefinite(
    IntegralIndefinite(Cos(Term("x")), Term("x"))
  )
  override def run(args: List[String]): IO[ExitCode] =
    for {
      r <- res
      _ = println(r)
    } yield ExitCode.Success
}
