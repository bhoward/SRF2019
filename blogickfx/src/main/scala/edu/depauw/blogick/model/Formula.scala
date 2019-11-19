package edu.depauw.blogick.model

import fastparse.Parsed.Success
import fastparse.Parsed.Failure

final case class UnificationException(msg: String) extends Exception(msg)

sealed trait Formula {
  def unify(other: Formula): Unit

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
  def unify(other: Formula): Unit = other match {
    case Implication(l, r) => left.unify(l); right.unify(r)
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }
  
  def render(precedence: Int): String = parenIf(3, precedence)(s"${left.render(3)} → ${right.render(2)}")
}

final case class Conjunction(left: Formula, right: Formula) extends Formula {
  def unify(other: Formula): Unit = other match {
    case Conjunction(l, r) => left.unify(l); right.unify(r)
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }
  
  def render(precedence: Int): String = parenIf(2, precedence)(s"${left.render(1)} ∧ ${right.render(2)}")
}

final case class Disjunction(left: Formula, right: Formula) extends Formula {
  def unify(other: Formula): Unit = other match {
    case Disjunction(l, r) => left.unify(l); right.unify(r)
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }

  def render(precedence: Int): String = parenIf(1, precedence)(s"${left.render(0)} ∨ ${right.render(1)}")
}

final case class Proposition(name: String) extends Formula {
  def unify(other: Formula): Unit = other match {
    case Proposition(n) if name == n => ()
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }

  def render(precedence: Int): String = name
}

final case class Variable(id: Int) extends Formula {
  var ref: Option[Formula] = None

  override def equals(that: Any): Boolean =
    ref match {
      case Some(f) => f.equals(that)
      case None => that match {
        case that: Variable => this eq that
        case _ => false
      }
    }

  def unify(other: Formula): Unit = ref match {
    case Some(f) => f.unify(other)
    case None => ref = Some(other)
  }

  def render(precedence: Int): String = ref match {
    case Some(f) => f.render(precedence)
    case None => s"_$id"
  }
}

final case class Negation(form: Formula) extends Formula {
  def unify(other: Formula): Unit = other match {
    case Negation(f) => form.unify(f)
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }

  def render(precedence: Int): String = s"¬${form.render(3)}"
}

final case object True extends Formula {
  def unify(other: Formula): Unit = other match {
    case True => ()
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }

  def render(precedence: Int): String = "⊤"
}

final case object False extends Formula {
  def unify(other: Formula): Unit = other match {
    case False => ()
    case Variable(_) => other.unify(this)
    case _ => throw UnificationException(s"Unable to unify $this with $other")
  }

  def render(precedence: Int): String = "⊥"
}

object Formula {
  var varId = 0

  def genVar(): Variable = {
    varId += 1
    Variable(varId)
  }

  def resetVars(): Unit = {
    varId = 0
  }

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