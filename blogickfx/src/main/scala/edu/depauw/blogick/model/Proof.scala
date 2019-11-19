package edu.depauw.blogick.model

final case class Binding(name: String, formula: Formula)

final case class BindingException(msg: String) extends Exception(msg)

sealed trait Proof {
  var formula: Formula = null

  def check(env: List[Binding]): Unit
}

final case class ImplIntro(hypothesis: Binding,  conclusion: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    conclusion.check(hypothesis :: env)
    formula = Implication(hypothesis.formula, conclusion.formula)
  }
}

final case class ImplElim(impl: Proof, arg: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    impl.check(env)
    arg.check(env)
    Implication(arg.formula, formula).unify(impl.formula)
  }
}

final case object TrueIntro extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = True
  }
}

final case class ConjIntro(first: Proof, second: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    first.check(env)
    second.check(env)
    formula = Conjunction(first.formula, second.formula)
  }
}

final case class ConjElimFirst(conj: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    val dummy = Formula.genVar()
    conj.check(env)
    Conjunction(formula, dummy).unify(conj.formula)
  }
}

final case class ConjElimSecond(conj: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    val dummy = Formula.genVar()
    conj.check(env)
    Conjunction(dummy, formula).unify(conj.formula)
  }
}

final case class DisjIntroLeft(arg: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    arg.check(env)
    formula = Disjunction(arg.formula, Formula.genVar())
  }
}

final case class DisjIntroRight(arg: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    arg.check(env)
    formula = Disjunction(Formula.genVar(), arg.formula)
  }
}

final case class DisjElim(disj: Proof, leftBind: Binding, leftCase: Proof, rightBind: Binding, rightCase: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    disj.check(env)
    leftCase.check(leftBind :: env)
    rightCase.check(rightBind :: env)
    Disjunction(leftBind.formula, rightBind.formula).unify(disj.formula)
    formula.unify(leftCase.formula)
    formula.unify(rightCase.formula)
  }
}

final case class FalseElim(falsum: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    falsum.check(env)
    False.unify(falsum.formula)
  }
}

final case class NegIntro(hypothesis: Binding, contradiction: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Negation(hypothesis.formula)
    contradiction.check(hypothesis :: env)
    False.unify(contradiction.formula)
  }
}

final case class NegElim(neg: Proof, arg: Proof) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = Formula.genVar()
    neg.check(env)
    arg.check(env)
    Negation(arg.formula).unify(neg.formula)
  }
}

final case class Use(binding: Binding) extends Proof {
  def check(env: List[Binding]): Unit = {
    // TODO should this just take a name and look up the binding?
    if (!env.contains(binding)) {
      throw BindingException(s"Binding not found: $binding")
    }
    formula = binding.formula
  }
}

final case class ToDo(form: Formula) extends Proof {
  def check(env: List[Binding]): Unit = {
    formula = form
  }
}