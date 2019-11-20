package edu.depauw.blogick.model

sealed trait CheckedProof {
  def formula: Formula
}

final case class CkImplIntro(formula: Formula, hypothesis: Binding,  conclusion: CheckedProof) extends CheckedProof

final case class CkImplElim(formula: Formula, impl: CheckedProof, arg: CheckedProof) extends CheckedProof

final case object CkTrueIntro extends CheckedProof {
  val formula: Formula = True
}

final case class CkConjIntro(formula: Formula, first: CheckedProof, second: CheckedProof) extends CheckedProof

final case class CkConjElimFirst(formula: Formula, conj: CheckedProof) extends CheckedProof

final case class CkConjElimSecond(formula: Formula, conj: CheckedProof) extends CheckedProof

final case class CkDisjIntroLeft(formula: Formula, arg: CheckedProof) extends CheckedProof

final case class CkDisjIntroRight(formula: Formula, arg: CheckedProof) extends CheckedProof

final case class CkDisjElim(formula: Formula, disj: CheckedProof,
    leftBind: Binding, leftCase: CheckedProof,
    rightBind: Binding, rightCase: CheckedProof) extends CheckedProof

final case class CkFalseElim(formula: Formula, falsum: CheckedProof) extends CheckedProof

final case class CkNegIntro(formula: Formula, hypothesis: Binding, contradiction: CheckedProof) extends CheckedProof

final case class CkNegElim(formula: Formula, neg: CheckedProof, arg: CheckedProof) extends CheckedProof

final case class CkUse(formula: Formula, binding: Binding) extends CheckedProof

final case class CkToDo(formula: Formula, env: List[Binding]) extends CheckedProof
