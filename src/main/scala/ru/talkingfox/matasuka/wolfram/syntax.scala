package ru.talkingfox.matasuka.wolfram

import ru.talkingfox.matasuka.math.MathExpression

object syntax {
  implicit class MathExprOps(val me: MathExpression) {
    def wolf(implicit interpreter: WolframInterpreter): String = interpreter.interprete(me)
  }

}
