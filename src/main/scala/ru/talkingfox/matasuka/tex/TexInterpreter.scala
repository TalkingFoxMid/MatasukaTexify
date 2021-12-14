package ru.talkingfox.matasuka.tex

import ru.talkingfox.matasuka.math.MathExpression
import ru.talkingfox.matasuka.math._
import ru.talkingfox.matasuka
import ru.talkingfox.matasuka.math.math_syntax.syntax._
import cats.syntax.all._
import mouse.all._
import ru.talkingfox.matasuka.math.Statements._

class TexInterpreter {
  implicit val ti: TexInterpreter = this
  val brackets = Set('(', '{')
  val Cobrackets = Set(')', '}')
  val ToTrim = Set(' ', '\n')
  val Operations = Set('*', '+', '-', '/')
  val NumberSymbols: Set[Char] = (0 to 9).map(_.toString.head).toSet ++ Set('.', ',')
  def bracketOf(char: Char): Char = char match {
    case ')' => '('
    case '}' => '{'
  }

  def texify(matrix: Matrix): String = "ab"

  def texifyStatement(stmt: Statement): String = stmt match {
    case expression: MathExpression => texify(expression)
    case Statements.Eqv(a, b) => s"${texifyStatement(a)} = ${texifyStatement(b)}"
    case Relation(a, b, sym) => s"${texifyStatement(a)} ${sym} ${texifyStatement(b)}"
    case Norm(a) => s"\\norm{${texifyStatement(a)}}"
    case Cases(css) => s"\\begin{cases}${css.map(texifyStatement).mkString("\\\\")} \\end{cases}"
  }

  def texify(expr: MathExpression): String = expr match {
    case Frac(m1, m2) => s"\\frac{${m1.texify}}{${m2.texify}}"
    case Number(a) => a match {
      case _ if a % 1 == 0 => s"${a.toInt}"
      case x => s"${x}"
    }
    case Multiply(exp) => {
      exp.map {
        case p@Plus(_) => s"(${p.texify})"
        case otherwise => otherwise.texify
      }.mkString(" ")
    }
    case Plus(exp) => (for {
      (term, index) <- exp.zipWithIndex
      str = term match {
        case a if index == 0 => s"${a.texify}"
        case Minus(a) => s"-${a.texify}"
        case a => s"+${a.texify}"
      }
    } yield str).mkString

    case Power(m1, m2) => m1 match {
      case p: Plus => s"(${{m1.texify}})^{${m2.texify}}"
      case _ => s"${{m1.texify}}^{${m2.texify}}"
    }
    case Sqrt(targer, p) => p match {
      case Number(2) =>s"\\sqrt{${targer.texify}}"
      case otherwise => s"\\sqrt[${otherwise.texify}]{${targer.texify}}"
    }
    case Term(term) => term
    case IntegralDefinite(IntegralIndefinite(core, d), brange, urange) =>
      s"\\int_{${brange.texify}}^{${urange.texify}}{${core.texify}d${d.texify}}"
    case IntegralIndefinite(core, d) => core match {
      case _: Plus => s"\\int{(${core.texify})d${d.texify}}"
      case _ => s"\\int{${core.texify}d${d.texify}}"
    }
    case Sin(target) => s"\\sin(${target.texify})"
    case Cos(target) => s"\\cos(${target.texify})"
    case Minus(a) => s"-${a.texify}"
    case EConst => s"e"
    case PIConst => s"\\pi"
    case Function(name, Nil, _, _) => name.texify
    case Function(name, args, a, _) =>
      s"${name.texify}(${args
        .map(arg => a.getOrElse(arg, arg))
        .map(_.texify)
        .mkString(", ")})"
    case Sum(body, i, beginV, endV) => body match {
      case p: Plus => s"\\sum_{${i.texify} = ${beginV.texify}}^${endV.texify}{(${body.texify})}"
      case _ => s"\\sum_{${i.texify} = ${beginV.texify}}^${endV.texify}{${body.texify}}"
    }

  }
  case class BracketLevel(lvl: Map[Char, Int]) {
    def applyChar(char: Char): BracketLevel =
      char match {
        case _ if brackets.contains(char) => BracketLevel(lvl ++ Map(char -> (lvl.getOrElse(char, 0) + 1)))
        case _ if Cobrackets.contains(char) => {
          val bracket = bracketOf(char)
          BracketLevel(lvl ++ Map(bracket -> (lvl.getOrElse(bracket, 0) - 1)))
        }
        case _ => this
      }
  }

  def zipWithBracketLevels(s: String): List[(Char, BracketLevel)] = {
    val (res, _) = s.foldLeft((List.empty[(Char, BracketLevel)], BracketLevel(Map.empty))) {
      case ((list, bracketLevel), char) => {
        ((char, bracketLevel.applyChar(char)) :: list, bracketLevel.applyChar(char))
      }
    }
    res.reverse
  }

  def extractTopLevelOperation(s: String, template: Char): List[String] = {
    val indicies = zipWithBracketLevels(s).zipWithIndex
      .collect {
        case ((char, BracketLevel(lvl)), i) if char == template && lvl.values.forall(_ == 0) => i
      }
    indicies.map(_ + 1).prepended(0)
      .zip(indicies.appended(s.length))
      .map {
        case (beg, end) => s.substring(beg, end)
      }
  }

  def untexify(s: String): MathExpression = {
    val trimmed = s.filterNot(ToTrim.contains)
    (trimmed != s).option(untexify(trimmed))
      .orElse {
        val noLeftRight = s.replace("\\left", "").replace("\\right", "")
        (noLeftRight != s).option(untexify(noLeftRight))
      }
      .orElse(s.isEmpty.option(Number(0)))
      .orElse(extractTopLevelOperation(s, '+').some.filter(_.length > 1)
        .collect {
          case head :: head2 :: next => Plus(
            untexify(head) :: untexify(head2) :: next.map(untexify)
          ).flatten
        })
        .orElse(
          extractTopLevelOperation(s, '-').some
            .collect {
              case head :: head2 :: next => Plus(
                untexify(head) ::
                  Minus(untexify(head2)).flatten ::
                  next.map(untexify).map(Minus(_)).map(_.flatten)
              ).flatten
            }
        )
        .orElse(
          extractTopLevelOperation(s, '*').some
            .collect {
              case head :: head2 :: next => Multiply(
                untexify(head) ::
                  untexify(head2) ::
                  next.map(untexify)
              ).flatten
            }
        )
        .orElse(emplaceMultipliesIfNeed(s).map(untexify))
        .orElse(
          brackets.contains(s.head).option(
            untexify(insideBrackets(s))
          )
        )
        .orElse((s.head == '-').option(Minus(untexify(s.tail)).flatten))
        .orElse(s.toDoubleOption.map(Number))
        .orElse(s.startsWith("\\cos").option(
          Cos(untexify(insideBrackets(s.drop(4)))))
        )
        .orElse(s.startsWith("\\sin").option(
          Sin(untexify(insideBrackets(s.drop(4)))))
        )
        .orElse(s.startsWith("\\pi").option(PIConst))
        .orElse(s.startsWith("\\frac").option(
          {
            val inside = insideBrackets(s.drop(5))
            val inside2 = insideBrackets(s.drop(5).drop(2).drop(inside.length))
            Frac(untexify(inside), untexify(inside2))
          }
        ))
        .getOrElse(Term(s))
  }
  def insideBrackets(from: String): String = {
    from.headOption.map { firstBracket =>
      zipWithBracketLevels(from)
        .tail
        .takeWhile {
          case (_, BracketLevel(lvl)) if lvl.getOrElse(firstBracket, 0) > 0 => true
          case _ => false
        }.map {
        case (c, _) => c
      }.mkString
    }.getOrElse(from)
  }

  def emplaceMultipliesHelper(chars: List[Char], acc: List[Char] = Nil, inFunc: Boolean = false): List[Char] = {
    val inFuncNew = chars.headOption.exists {
      h => (h == '\\' || inFunc) && !(brackets.contains(h))
    }
    chars match {
      case first :: second :: others =>  {
        val setMultiplySymbol =
          (NumberSymbols.contains(first) && !(NumberSymbols ++ Operations).contains(second) && !Cobrackets.contains(second)) ||
            (second == '(' && !Operations.contains(first) && first != '(')
        if (setMultiplySymbol && !inFunc)
          emplaceMultipliesHelper(second :: others, '*' :: first :: acc, inFuncNew)
        else
          emplaceMultipliesHelper(second :: others, first :: acc, inFuncNew)
      }
      case Nil => acc
      case x => emplaceMultipliesHelper(Nil, x ::: acc)

    }
  }

  def emplaceMultipliesIfNeed(s: String): Option[String] = {
    val newString = emplaceMultipliesHelper(s.toCharArray.toList).reverse.mkString
    (newString != s).option(newString)
  }

}
object TexInterpreter extends App {
  val ti = new TexInterpreter
  val res = ti.untexify("3* (\\pi^2-8)")
  println(res)
}