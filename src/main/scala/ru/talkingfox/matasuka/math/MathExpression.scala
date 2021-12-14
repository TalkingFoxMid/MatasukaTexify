package ru.talkingfox.matasuka.math

sealed trait MathExpression

case class Frac(m1: MathExpression, m2: MathExpression) extends MathExpression {
    def upMap(f: MathExpression => MathExpression): Frac = Frac(f(m1), m2)
    def downMap(f: MathExpression => MathExpression): Frac = Frac(m1, f(m2))
}

case class Number(a: BigDecimal) extends MathExpression {
    def map(f: BigDecimal => BigDecimal) = Number(f(a))
}

case class Term(symbol: String) extends MathExpression

case class Multiply(exp: List[MathExpression]) extends MathExpression

case class Sum(exp: List[MathExpression]) extends MathExpression

case class Power[A <: MathExpression, P <: MathExpression](m1: A, m2: P) extends MathExpression {
    def map[B <: MathExpression](f: A => B): Power[B, P] = Power[B, P](f(m1), m2)
    def mapPower[Q <: MathExpression](f: P => Q): Power[A, Q] = Power[A, Q](m1, f(m2))
}

case class Sqrt[A <: MathExpression, P <: MathExpression](target: A, p: P) extends MathExpression {
    def map[B <: MathExpression](f: A => B): Sqrt[B, P] = Sqrt[B, P](f(target), p)
    def mapPower[Q <: MathExpression](f: P => Q): Sqrt[A, Q] = Sqrt[A, Q](target, f(p))
}

case class Text(text: String) extends MathExpression