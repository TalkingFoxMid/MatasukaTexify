package ru.talkingfox.matasuka.math.math_syntax

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import ru.talkingfox.matasuka.MathMacrox
import ru.talkingfox.matasuka.markdown.Markdown
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.WolframSolver

import scala.language.experimental.macros

trait MathSyntax {
  implicit def mathExpressionMathable[A <: MathExpression]: Mathable[A, A] = identity
  implicit def numericMathable[A: Numeric]: Mathable[A, Number] = Numeric[A].toDouble(_).asNum
//  implicit val intMathable: Mathable[Int, Number] = _.toDouble.asNum

  def frac[A <: MathExpression, B <: MathExpression](a: A)(b: B): Frac[A, B] =
    Frac(a, b)

  implicit class MathOps[A](i: A) {
    def as[B]: B = macro MathMacrox.toMathExpression[A, B]

    def term: Term = Term(i.toString)

    def func: Function = Function(Term(i.toString), Nil)

    //    def *(other: MathExpression): Multiply = macro MathMacrox.multiply[A]
  }

  implicit class NumberOps[A: Numeric](i : A) {
    def asNum: Number = Number(implicitly[Numeric[A]].toDouble(i))
  }

  implicit class MathInterpolators(val sc: StringContext) {
    def term(args: Any*): Term = Term(sc.parts.head)
  }

  implicit class IntegralIndOps[A <: MathExpression](integral: IntegralIndefinite[A]) {
    def solve[F[_]](implicit solver: WolframSolver[F]): F[MathExpression] = {
      solver.solveIndefinite(integral)
    }
  }

  implicit class IntegralDefOps[A <: MathExpression, B <: MathExpression, C <: MathExpression](integral: IntegralDefinite[A, B, C]) {
    def solve[F[_]](implicit solver: WolframSolver[F]): F[MathExpression] = {
      solver.solveDefinite(integral)
    }
  }

  implicit class MathExprIOOps[A <: MathExpression](expr: IO[A])(implicit runtime: IORuntime) {
    def texify(implicit interpreter: TexInterpreter): String = interpreter.texify(expr.unsafeRunSync())

    def md(implicit interpreter: TexInterpreter): Markdown[A] = new Markdown[A](expr.unsafeRunSync(), expr.texify :: Nil)
  }

  implicit class MatrixOps(matrix: Matrix) {
    def md(implicit interpreter: TexInterpreter): Markdown[Matrix] =
      new Markdown[Matrix](matrix, interpreter.texify(matrix) :: Nil)
  }

  implicit class MathExpressionSyntax[I :Mathable[*, A], A <: MathExpression](a: I){
    val expr = implicitly[Mathable[I, A]].mathify(a)
    def *[B: Numeric](num: B): Multiply =
      Multiply(Number(Numeric[B].toDouble(num)) :: expr :: Nil)

    def -[B: Numeric](num: B): Plus =
      Plus(expr :: Minus(Number(Numeric[B].toDouble(num))) :: Nil)

    def +[B: Numeric](num: B): Plus =
      Plus(Number(Numeric[B].toDouble(num)) :: expr :: Nil)

    def ^[B: Numeric](num: B): Power[Number, A] =
      Power(Number(Numeric[B].toDouble(num)), expr)

    def mapTree(pf: PartialFunction[MathExpression, MathExpression]): MathExpression =
      MathTreeUtils.mapTree(expr)(pf)

    def withValue(T: Term, me: Double): MathExpression =
      MathTreeUtils.mapTree(expr) {
        case t@Term(_) if t == T => Number(me)
      }

    def withExpression(T: Term, me: MathExpression): MathExpression =
      MathTreeUtils.mapTree(expr) {
        case t@Term(_) if t == T => me
      }

    def solveNumeric[F[_]](implicit solverNumeric: SolverNumeric[F]): F[Double] =
      solverNumeric.solveNumeric(expr)

    def texify(implicit interpreter: TexInterpreter): String = interpreter.texify(expr)

    def *(other: MathExpression): Multiply = (expr, other) match {
      case (Multiply(lst), _) => Multiply(other :: lst)
      case (_, Multiply(lst)) => Multiply(expr :: lst)
      case (a, b) => Multiply(a :: b :: Nil)
    }

    def +(other: MathExpression): Plus = (expr, other) match  {
      case (Plus(lst), _) => Plus(other :: lst)
      case (_, Plus(lst)) => Plus(expr :: lst)
      case (a, b) => Plus(a :: b :: Nil)
    }

    def -(other: MathExpression): Plus = (expr, other) match  {
      case (a, b) => Plus(a :: Minus(b) :: Nil)
    }

    def ^[B <: MathExpression](other: B): Power[A, B] = (expr, other) match  {
      case (a, b) => Power[A, B](a, b)
    }

    def frac[B : Mathable[*, C], C <: MathExpression](other: B): Frac[A, C]  = Frac(expr, implicitly[Mathable[B, C]].mathify(other))

    def pow[B : Mathable[*, C], C <: MathExpression](other: B): Power[A, C] = Power(expr, Mathable[B, C].mathify(other))

    def sqrt2: Sqrt[A, Number] = Sqrt(expr, Number(2))

    def integrate(term: Term): IntegralIndefinite[A] = IntegralIndefinite(expr, term)
  }
}