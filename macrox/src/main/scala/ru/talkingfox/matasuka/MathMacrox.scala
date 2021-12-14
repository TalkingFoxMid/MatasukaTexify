package ru.talkingfox.matasuka

import scala.reflect.macros.blackbox

class MathMacrox(val c: blackbox.Context) {
    import c.universe._

    def toMathExpression[A, B: WeakTypeTag]: c.Expr[B] = {
        val (args, fromTp) = c.prefix.tree match {
            case Apply(TypeApply(_, List(tp)), v) => {
                (v, tp.toString())
            }
        }

        val toTp = implicitly[WeakTypeTag[B]].tpe.toString() 
        val applyTree = (fromTp, toTp) match {
            case ("Int", "ru.talkingfox.matasuka.math.Number") => 
                q"ru.talkingfox.matasuka.math.Number"

            case ("String", "ru.talkingfox.matasuka.math.Term") =>
                q"ru.talkingfox.matasuka.math.Term"

            case _ => c.abort(c.enclosingPosition, "Unknown type for toMathExpression")
        }
        c.Expr[B](Apply(applyTree, args))
    }

    def genericMathMultiply(other: c.Expr[MathExpression]): c.Expr[Multiply] = {
        c.Expr[Multiply](q"Multiply(Nil)")
    }

}