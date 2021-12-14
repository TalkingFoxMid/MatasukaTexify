package ru.talkingfox.matasuka.math.math_syntax

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import ru.talkingfox.matasuka.MathMacrox
import ru.talkingfox.matasuka.markdown.Markdown
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka.tex.TexInterpreter
import ru.talkingfox.matasuka.wolfram.WolframSolver

import scala.language.experimental.macros

object syntax extends MathSyntax with NumberSyntax with StatementSyntax

