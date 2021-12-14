package ru.talkingfox.matasuka.markdown
import cats.syntax.all._
import ru.talkingfox.matasuka.markdown.syntax._

trait Symbols {
    case class Items(items: List[Markdown[Unit]]) {
        private val counter = items.size + 1

        def withItem[A](a: Markdown[A]): Items
            = Items(
            (s"${counter.toString}) ".md)
              .flatMap(_ => a)
              .map(_ => ()) :: items
            )

        def md: Markdown[Unit] =
            items.reverse.foldLeft(begin)((a, b) => a.flatMap(_ => b))
              .flatMap(_ => newLine)
    }
    object Items {
        def apply(): Items = Items(Nil)
    }
    def text(text: String): Markdown[String] = new Markdown[String](text, text :: Nil)
}
