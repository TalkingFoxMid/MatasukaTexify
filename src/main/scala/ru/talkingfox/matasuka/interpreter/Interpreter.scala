package ru.talkingfox.matasuka.interpreter

import ru.talkingfox.matasuka.math.MathExpression
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka
import ru.talkingfox.matasuka.math.syntax._

object Interpreter {
  def texify(expr: MathExpression): String = expr match {
      case Frac(m1, m2) => s"\\frac{${m1.texify}}{${m2.texify}}"
      case Number(a) => s"$a"
      case Multiply(exp) => s"${exp.map(_.texify).mkString("*")}"
      case Sum(exp) => s"${exp.map(_.texify).mkString("+")}"
      case Power(m1, m2) => s"${{m1.texify}}^{${m2.texify}}"
      case Sqrt(targer, p) => s"\\sqrt[$p]{${targer.texify}}"
      case Text(text) => text
      case Term(term) => term
  }
}
