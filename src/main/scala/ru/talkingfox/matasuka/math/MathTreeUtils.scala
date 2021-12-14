package ru.talkingfox.matasuka.math

object MathTreeUtils {
  def mapTree(expr: MathExpression)(f: PartialFunction[MathExpression, MathExpression]): MathExpression =
    f.lift(expr).getOrElse {
      expr match {
        case Frac(m1, m2) => Frac(mapTree(m1)(f), mapTree(m2)(f))
        case Number(a) => Number(a)
        case Minus(a) => Minus(mapTree(a)(f))
        case Term(symbol) => Term(symbol)
        case Multiply(exprs) => Multiply(exprs.map(mapTree(_)(f)))
        case Plus(exprs) => Plus(exprs.map(mapTree(_)(f)))
        case Power(m1, m2) => Power(mapTree(m1)(f), mapTree(m2)(f))
        case Sqrt(target, p) => Sqrt(mapTree(target)(f), mapTree(p)(f))
        case Sin(target) => Sin(mapTree(target)(f))
        case Cos(target) => Cos(mapTree(target)(f))
        case IntegralIndefinite(core, d) => IntegralIndefinite(mapTree(core)(f), d)
        case IntegralDefinite(IntegralIndefinite(core, d), brange, urange) => IntegralDefinite(
          IntegralIndefinite(mapTree(core)(f), d),
          mapTree(brange)(f),
          mapTree(urange)(f)
        )
        case EConst => EConst
        case PIConst => PIConst
        case Function(name, args, a, b) => {
          val newMap = args.map(arg => (arg, mapTree(arg)(f))).toMap
          Function(name, args, a ++newMap, b.map(mapTree(_)(f)))
        }
        case Sum(body, i, beginV, endV) =>
          Sum(mapTree(body)(f), i, mapTree(beginV)(f), mapTree(endV)(f))
      }
    }
}
