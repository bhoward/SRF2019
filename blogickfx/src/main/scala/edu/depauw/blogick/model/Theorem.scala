package edu.depauw.blogick.model

class Theorem(val name: String, val formula: Formula, val proof: Proof) {
  val checkedProof: CheckedProof = proof.check.runA(Environment.Empty).value

  val _ = checkedProof.formula.unify(formula)

  override def toString: String = s"$name: $formula = $proof"
}

object Theorem {
  import fastparse._
  import fastparse.SingleLineWhitespace._
  import fastparse.Parsed.Success
  import fastparse.Parsed.Failure

  // TODO move these into a common utils object...
  private def idStart(c: Char): Boolean = c.isUnicodeIdentifierStart
  private def idPart(c: Char): Boolean =
    c.isUnicodeIdentifierPart || (c == '\'')

  private def id[_: P]: P[String] = P(
    (CharPred(idStart) ~~ CharsWhile(idPart).?).!
  )

  def parser[_: P]: P[Theorem] = P(
    (id ~ ":" ~ Formula.parser ~ "=" ~ Proof.parser).map {
      case (name, formula, proof) => new Theorem(name, formula, proof)
    }
  )

  def fromString(s: String): Theorem = {
    def topLevel[_: P]: P[Theorem] = P(parser ~ End)

    parse(s, topLevel(_)) match {
      case Success(value, _) => value
      case f @ Failure(label, index, extra) => sys.error(f.trace().msg) // TODO improve this
    }
  }
}