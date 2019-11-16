package edu.depauw.blogick.model
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

sealed trait Formula {
  def render(precedence: Int): String
  override def toString: String = render(0)

  def parenIf(level: Int, precedence: Int)(s: String): String =
  if (precedence >= level) {
    "(" + s + ")"
  } else {
    s
  }
}

final case class Implication(left: Formula, right: Formula) extends Formula {
  def render(precedence: Int): String = parenIf(3, precedence)(s"${left.render(3)} → ${right.render(2)}")
}
final case class Conjunction(left: Formula, right: Formula) extends Formula {
  def render(precedence: Int): String = parenIf(2, precedence)(s"${left.render(1)} ∧ ${right.render(2)}")
}
final case class Disjunction(left: Formula, right: Formula) extends Formula {
  def render(precedence: Int): String = parenIf(1, precedence)(s"${left.render(0)} ∨ ${right.render(1)}")
}
final case class Proposition(name: String) extends Formula {
  def render(precedence: Int): String = name
}
final case class Negation(form: Formula) extends Formula {
  def render(precedence: Int): String = s"¬${form.render(3)}"
}
final case object True extends Formula {
  def render(precedence: Int): String = "⊤"
}
final case object False extends Formula {
  def render(precedence: Int): String = "⊥"
}

object Formula {
  def apply(s: String): Formula = {
    import fastparse._, SingleLineWhitespace._

    def idStart(c: Char): Boolean = c.isUnicodeIdentifierStart
    def idPart(c: Char): Boolean = c.isUnicodeIdentifierPart || (c == '\'')

    def bottom[_: P]: P[Formula] = P(
      ("False" | "⊥").!.map(_ => False)
    )

    def top[_: P]: P[Formula] = P(
      ("True" | "⊤").!.map(_ => True)
    )

   def prop[_: P]: P[Formula] = P(
      (CharPred(idStart)~~CharsWhile(idPart).?).!.map(Proposition(_))
    )

    def parens[_: P]: P[Formula] = P(
      "(" ~/ disj ~ ")"
    )

    def negFactor[_: P]: P[Formula] = P(
      (("~" | "¬") ~/ factor).map(Negation(_))
    )

    def factor[_: P]: P[Formula] = P(
      parens | negFactor | bottom | top | prop 
    )

    def impl[_: P]: P[Formula] = P(
      (factor ~ (("->" | "→") ~/ factor).rep).map {
        case (f, fs) => (f +: fs).reduceRight(Implication(_, _))
      }
    )

    def conj[_: P]: P[Formula] = P(
      (impl ~ (("&" | "/\\" | "∧") ~/ impl).rep).map {
        case (f, fs) => (f +: fs).reduceLeft(Conjunction(_, _))
      }
    )

    def disj[_: P]: P[Formula] = P(
      (conj ~ (("|" | "\\/" | "∨") ~/ conj).rep).map {
        case (f, fs) => (f +: fs).reduceLeft(Disjunction(_, _))
      }
    )

    def formula[_: P]: P[Formula] = P(disj ~ End)

    parse(s, formula(_)) match {
      case Success(value, _) => value
      case f @ Failure(label, index, extra) => sys.error(f.trace().msg) // TODO improve this
    }
  }
}