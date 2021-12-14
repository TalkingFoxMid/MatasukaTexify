package ru.talkingfox.matasuka.math

import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka.interpreter.Interpreter
import ru.talkingfox.matasuka.markdown.Markdown
import scala.language.experimental.macros
import cats.syntax.option._
import ru.talkingfox.matasuka.MathMacrox

object syntax {
  implicit class MathOps[A](i: A) {
    def as[B]: B = macro MathMacrox.toMathExpression[A, B]

    def *(other: MathExpression): Multiply = macro MathMacrox.multiply[A]
  }
    
  implicit class MathInterpolators(val sc: StringContext) extends AnyVal {
    def math(args: Any*): MathExpression = Text(sc.parts.head)

    def term(args: Any*): Term = Term(sc.parts.head)
  }

  implicit class MathExprOps[A <: MathExpression](expr: A) {
      def texify: String = Interpreter.texify(expr)

      def md: Markdown[A] = new Markdown[A](expr, expr.texify :: Nil) 

      def *(other: MathExpression): Multiply = (expr, other) match {
          case (Multiply(lst), _) => Multiply(other :: lst)
          case (_, Multiply(lst)) => Multiply(expr :: lst)
          case (a, b) => Multiply(a :: b :: Nil)
      }

      def +(other: MathExpression): Sum = (expr, other) match  {
          case (Sum(lst), _) => Sum(other :: lst)
          case (_, Sum(lst)) => Sum(expr :: lst)
          case (a, b) => Sum(a :: b :: Nil)
      }

      def ^[B <: MathExpression](other: B): Power[A, B] = (expr, other) match  {
          case (a, b) => Power[A, B](a, b)
      }
      
      def pow[P <: MathExpression](other: P): Power[A, P] = Power(expr, other)
      
      def sqrt2: Sqrt[A, Number] = Sqrt(expr, Number(2))
      
  }
}
