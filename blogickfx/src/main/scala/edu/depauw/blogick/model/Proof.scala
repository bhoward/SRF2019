package edu.depauw.blogick.model

final case class Binding(name: String, formula: Formula)

sealed trait Proof {
  def formula: Formula
}

final case class ImplIntro(hypothesis: Binding,  conclusion: Proof) extends Proof {
  def formula: Formula = Implication(hypothesis.formula, conclusion.formula)
}

final case class ImplElim(impl: Proof, arg: Proof) extends Proof {
  def formula: Formula = {
    val result = Variable(0) // TODO generate unique numbers?
    Implication(arg.formula, result).unify(impl.formula)
    result
  }
}

final case object TrueIntro extends Proof {
  def formula: Formula = True
}

final case class ConjIntro(first: Proof, second: Proof) extends Proof {
  def formula: Formula = Conjunction(first.formula, second.formula)
}

final case class ConjElimFirst(conj: Proof) extends Proof {
  def formula: Formula = {
    val result = Variable(0)
    val dummy = Variable(1)
    Conjunction(result, dummy).unify(conj.formula)
    result
  }
}

final case class ConjElimSecond(conj: Proof) extends Proof {
  def formula: Formula = {
    val result = Variable(0)
    val dummy = Variable(1)
    Conjunction(dummy, result).unify(conj.formula)
    result
  }
}

final case class DisjIntroLeft(arg: Proof) extends Proof {
  def formula: Formula = Disjunction(arg.formula, Variable(0))
}

final case class DisjIntroRight(arg: Proof) extends Proof {
  def formula: Formula = Disjunction(Variable(0), arg.formula)
}

final case class DisjElim(disj: Proof, leftBind: Binding, leftCase: Proof, rightBind: Binding, rightCase: Proof) extends Proof {
  def formula: Formula = {
    val result = Variable(0)
    Disjunction(leftBind.formula, rightBind.formula).unify(disj.formula)
    result.unify(leftCase.formula)
    result.unify(rightCase.formula)
    result
  }
}

final case class FalseElim(falsum: Proof) extends Proof {
  def formula: Formula = {
    False.unify(falsum.formula)
    Variable(0)
  }
}

final case class NegIntro(hypothesis: Binding, contradiction: Proof) extends Proof {
  def formula: Formula = {
    False.unify(contradiction.formula)
    Negation(hypothesis.formula)
  }
}

final case class NegElim(neg: Proof, arg: Proof) extends Proof {
  def formula: Formula = {
    Negation(arg.formula).unify(neg.formula)
    Variable(0)
  }
}

final case class Use(binding: Binding) extends Proof {
  def formula: Formula = binding.formula
}

final case class ToDo(formula: Formula) extends Proof