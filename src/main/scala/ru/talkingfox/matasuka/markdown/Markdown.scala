package ru.talkingfox.matasuka.markdown

import cats.Monad

trait Markdownable[A] {
    def md(a: A): Markdown[A]
}
object Markdownable {
    def apply[A](implicit markdownable: Markdownable[A]): Markdownable[A] =
        markdownable
}

class Markdown[+A](val a: A, val prevs: List[String] = Nil) {
    private def withPrevs(s: List[String]): Markdown[A] = 
        new Markdown[A](a, s ::: prevs)

    def flatMap[B](f: A => Markdown[B]): Markdown[B] = {
        f(a).withPrevs(prevs)
    }
//    def flatMap[B: Markdownable](f: A => B): Markdown[B] = {
//        Markdownable[B].md(f(a))
//    }
    def +[B](other: Markdown[B]): Markdown[(A, B)] =
    new Markdown[(A, B)]((a, other.a),
        (for {
            h1 <- prevs.headOption
            h2 <- other.prevs.headOption
        } yield (h1 + h2) :: prevs.tail).getOrElse(Nil)
    )
    def map[B](f: A => B): Markdown[B] =
        new Markdown[B](f(a), prevs)


    def compile: String = prevs.mkString("\n")
}
