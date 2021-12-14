package ru.talkingfox.matasuka
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka.math.syntax._
import ru.talkingfox.matasuka.markdown.Symbols._
object Main extends scala.App {
    val result = for {
        _ <- begin
        x = "x".as[Term]
        y = "y".as[Term]
        _ <- (2 * y).md
    } yield ()

    println(result.compile)
}
