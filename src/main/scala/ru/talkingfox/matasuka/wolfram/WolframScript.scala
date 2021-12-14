package ru.talkingfox.matasuka.wolfram

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Sync

import sys.process._

class WolframScript[F[_]: Sync] {
  def solveTeX(code: String): F[String] = {
    Sync[F].delay(s"wolframscript -code 'ToString[TeXForm[${code}]]'".!!)
  }

}

