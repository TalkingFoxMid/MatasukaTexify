package ru.talkingfox.matasuka.shared

import cats.effect.IO
import cats.effect.unsafe.IORuntime

trait SharedSyntax {
  implicit class IOOps[A](io: IO[A]) {
    def await(implicit rntm: IORuntime): A =
      io.unsafeRunSync()
  }
}
