package ru.talkingfox.matasuka.math

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.{EquationSolver, WolframInterpreter, WolframScript, WolframSolver}
trait EndofunctorionIO {
  implicit val ioRuntime = global
  implicit val wolframScript = new WolframScript[IO]
  implicit val interpreter = new WolframInterpreter
  implicit val texInterpreter = new TexInterpreter
  implicit val solver = new WolframSolver[IO]
  implicit val numSolver = new SolverNumeric[IO]
  implicit val eqSolver = new EquationSolver[IO]
}
