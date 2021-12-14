package ru.talkingfox.matasuka.funcan
import ru.talkingfox.matasuka.syntax.all._

object U8 extends App {
  val result = for {
    _ <- begin

  } yield ()
  println(result.compile)
}
