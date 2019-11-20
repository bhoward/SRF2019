package edu.depauw.blogick.model

final case class Binding(name: String, formula: Formula)

final case class ProofCheckException(msg: String) extends Exception(msg)

sealed trait Proof {
  def check(env: List[Binding]): CheckedProof
}

final case class ImplIntro(hypothesis: Binding,  conclusion: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val cc = conclusion.check(hypothesis :: env)
    val formula = Implication(hypothesis.formula, cc.formula)
    CkImplIntro(formula, hypothesis, cc)
  }
}

final case class ImplElim(impl: Proof, arg: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val ci = impl.check(env)
    val ca = arg.check(env)
    Implication(ca.formula, formula).unify(ci.formula)
    CkImplElim(formula, ci, ca)
  }
}

final case object TrueIntro extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    CkTrueIntro
  }
}

final case class ConjIntro(first: Proof, second: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val cf = first.check(env)
    val cs = second.check(env)
    val formula = Conjunction(cf.formula, cs.formula)
    CkConjIntro(formula, cf, cs)
  }
}

final case class ConjElimFirst(conj: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val dummy = Formula.genVar()
    val cc = conj.check(env)
    Conjunction(formula, dummy).unify(cc.formula)
    CkConjElimFirst(formula, cc)
  }
}

final case class ConjElimSecond(conj: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val dummy = Formula.genVar()
    val cc = conj.check(env)
    Conjunction(dummy, formula).unify(cc.formula)
    CkConjElimSecond(formula, cc)
  }
}

final case class DisjIntroLeft(arg: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val ca = arg.check(env)
    val formula = Disjunction(ca.formula, Formula.genVar())
    CkDisjIntroLeft(formula, ca)
  }
}

final case class DisjIntroRight(arg: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val ca = arg.check(env)
    val formula = Disjunction(Formula.genVar(), ca.formula)
    CkDisjIntroRight(formula, ca)
  }
}

final case class DisjElim(disj: Proof, leftBind: Binding, leftCase: Proof, rightBind: Binding, rightCase: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val cd = disj.check(env)
    val cl = leftCase.check(leftBind :: env)
    val cr = rightCase.check(rightBind :: env)
    Disjunction(leftBind.formula, rightBind.formula).unify(cd.formula)
    formula.unify(cl.formula)
    formula.unify(cr.formula)
    CkDisjElim(formula, cd, leftBind, cl, rightBind, cr)
  }
}

final case class FalseElim(falsum: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val cf = falsum.check(env)
    False.unify(cf.formula)
    CkFalseElim(formula, cf)
  }
}

final case class NegIntro(hypothesis: Binding, contradiction: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Negation(hypothesis.formula)
    val cc = contradiction.check(hypothesis :: env)
    False.unify(cc.formula)
    CkNegIntro(formula, hypothesis, cc)
  }
}

final case class NegElim(neg: Proof, arg: Proof) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    val formula = Formula.genVar()
    val cn = neg.check(env)
    val ca = arg.check(env)
    Negation(ca.formula).unify(cn.formula)
    CkNegElim(formula, cn, ca)
  }
}

final case class Use(binding: Binding) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    // TODO should this just take a name and look up the binding?
    if (!env.contains(binding)) {
      throw ProofCheckException(s"Binding not found: $binding")
    }
    CkUse(binding.formula, binding)
  }
}

final case class ToDo(formula: Formula) extends Proof {
  def check(env: List[Binding]): CheckedProof = {
    CkToDo(formula, env)
  }
}