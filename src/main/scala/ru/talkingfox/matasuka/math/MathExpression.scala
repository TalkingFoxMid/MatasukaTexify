package ru.talkingfox.matasuka.math

import cats.syntax.all._
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.WolframSolver

trait Mathable[A, +B <: Statement] {
    def mathify(a: A): B
}
object Mathable {
    def apply[A, B <: Statement](implicit mathable: Mathable[A, B]): Mathable[A, B] = implicitly[Mathable[A, B]]
}

sealed trait MathExpression extends Statement

case class Frac[A <: MathExpression, B <: MathExpression](m1: A, m2: B) extends MathExpression {
    def upMap[C <: MathExpression](f: A => C): Frac[C, B] = Frac(f(m1), m2)
    def downMap[C <: MathExpression](f: B => C): Frac[A, C] = Frac(m1, f(m2))
}

case class Number(a: Double) extends MathExpression {
    def map(f: Double => Double): Number = Number(f(a))
}

case class Minus[A <: MathExpression](a: A) extends MathExpression {
    def flatten: MathExpression = a match {
        case Plus(exprs) => Plus(exprs.map(Minus(_).flatten))
        case Minus(other) => other
        case otherwise => Minus(otherwise)
    }
}

case class Term(symbol: String) extends MathExpression {
    def i[O: Mathable[*, MathExpression]](ex: O)(implicit texInterpreter: TexInterpreter): Term =
        Term(s"${symbol}_{${texInterpreter.texify(Mathable[O, MathExpression].mathify(ex))}}")

    def prefix(pr: Term): Term =
        Term(s"${pr.symbol} ${symbol}")

    def func: Function = Function(this, Nil)

    def markify: Term =
        Term(s"${symbol}'")
}

case class Function(name: Term, args: List[Term], termValues: Map[Term, MathExpression] = Map.empty, expr: Option[MathExpression] = None) extends MathExpression {
    def mapName(f: Term => Term): Function =
        copy(name = f(name))
    def withValue(term: Term, value: MathExpression): Function = copy(termValues = termValues ++ Map(term -> value))


}

case class Multiply(exprs: List[MathExpression]) extends MathExpression {
    def flatMap(exp: MathExpression => List[MathExpression]): Multiply =
        Multiply(exprs.flatMap(exp))

    def flatten: Multiply = this.flatMap {
        case Multiply(others) => others
        case Number(a) if a == 0 => Nil
        case otherwise => otherwise :: Nil
    }
}

case class Plus(exprs: List[MathExpression]) extends MathExpression {
    def flatMap(exp: MathExpression => List[MathExpression]): Plus =
        Plus(exprs.flatMap(exp))

    def flatten: Plus = this.flatMap {
        case Plus(others) => others
        case Number(a) if a == 0 => Nil
        case otherwise => otherwise :: Nil
    }
}

case class Power[A <: MathExpression, P <: MathExpression](m1: A, m2: P) extends MathExpression {
    def map[B <: MathExpression](f: A => B): Power[B, P] = Power[B, P](f(m1), m2)
    def mapPower[Q <: MathExpression](f: P => Q): Power[A, Q] = Power[A, Q](m1, f(m2))
}

case class Sqrt[A <: MathExpression, P <: MathExpression](target: A, p: P) extends MathExpression {
    def map[B <: MathExpression](f: A => B): Sqrt[B, P] = Sqrt[B, P](f(target), p)
    def mapPower[Q <: MathExpression](f: P => Q): Sqrt[A, Q] = Sqrt[A, Q](target, f(p))
}


case class Sin[A <: MathExpression](target: A) extends MathExpression

case class Cos[A <: MathExpression](target: A) extends MathExpression

case class Sum(body: MathExpression,
               i: Term,
               beginV: MathExpression,
               endV: MathExpression) extends MathExpression


case class IntegralIndefinite[A <: MathExpression](core: A, d: Term) extends MathExpression {
    def setBounds(from: MathExpression, to: MathExpression): IntegralDefinite[A, MathExpression, MathExpression] =
        IntegralDefinite(this, from, to)
}

case class IntegralDefinite[
  A <: MathExpression,
  BR <: MathExpression,
  UR <: MathExpression](
    integralIndefinite: IntegralIndefinite[A],
    brange: BR,
    urange: UR
  ) extends MathExpression {
    def unBound: IntegralIndefinite[A] =
        integralIndefinite
}

case object EConst extends MathExpression

case object PIConst extends MathExpression