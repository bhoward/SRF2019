package edu.depauw.blogick.model

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
  
  def render(precedence: Int): String = right match {
    case False => s"¬${left.render(3)}"
    case _ => parenIf(3, precedence)(s"${left.render(3)} → ${right.render(2)}")
  }
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
        case v: Variable => this.id == v.id
        case _ => false
      }
    }

  // TODO also override hashCode, ...?

  def unify(other: Formula): Unit = ref match {
    case Some(f) => f.unify(other)
    case None => ref = Some(other)
  }

  def render(precedence: Int): String = ref match {
    case Some(f) => f.render(precedence)
    case None => s"_$id"
  }
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
  import fastparse._
  import fastparse.SingleLineWhitespace._
  import fastparse.Parsed.Success
  import fastparse.Parsed.Failure
    
  private def idStart(c: Char): Boolean = c.isUnicodeIdentifierStart
  private def idPart(c: Char): Boolean = c.isUnicodeIdentifierPart || (c == '\'')

  private def bottom[_: P]: P[Formula] = P(
    ("False" | "⊥").!.map(_ => False)
  )

  private def top[_: P]: P[Formula] = P(
    ("True" | "⊤").!.map(_ => True)
  )

  private def prop[_: P]: P[Formula] = P(
    (CharPred(idStart)~~CharsWhile(idPart).?).!.map(Proposition(_))
  )

  private def parens[_: P]: P[Formula] = P(
    "(" ~/ parser ~ ")"
  )

  private def negFactor[_: P]: P[Formula] = P(
    (("~" | "¬") ~/ factor).map(Implication(_, False))
  )

  private def factor[_: P]: P[Formula] = P(
    parens | negFactor | bottom | top | prop 
  )

  private def impl[_: P]: P[Formula] = P(
    (factor ~ (("->" | "→") ~/ factor).rep).map {
      case (f, fs) => (f +: fs).reduceRight(Implication(_, _))
    }
  )

  private def conj[_: P]: P[Formula] = P(
    (impl ~ (("&" | "/\\" | "∧") ~/ impl).rep).map {
      case (f, fs) => (f +: fs).reduceLeft(Conjunction(_, _))
    }
  )

  def parser[_: P]: P[Formula] = P(
    (conj ~ (("|" | "\\/" | "∨") ~/ conj).rep).map {
      case (f, fs) => (f +: fs).reduceLeft(Disjunction(_, _))
    }
  )

  def fromString(s: String): Formula = {
    def topLevel[_: P]: P[Formula] = P(parser ~ End)

    parse(s, topLevel(_)) match {
      case Success(value, _) => value
      case f @ Failure(label, index, extra) => sys.error(f.trace().msg) // TODO improve this
    }
  }
}