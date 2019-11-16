package edu.depauw.blogick.model

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
final case object True extends Formula {
  def render(precedence: Int): String = "T"
}
final case object False extends Formula {
  def render(precedence: Int): String = "F"
}