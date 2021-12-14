package ru.talkingfox.matasuka.markdown

import cats.Monad

class Markdown[A](a: A, prevs: List[String] = Nil) {    
    private def withPrevs(s: List[String]): Markdown[A] = 
        new Markdown[A](a, s ::: prevs)

    def flatMap[B](f: A => Markdown[B]): Markdown[B] = {
        f(a).withPrevs(prevs)
    }
       
    def map[B](f: A => B): Markdown[B] =
        new Markdown[B](f(a), prevs)

    def compile: String = prevs.mkString("\n")
        
}
