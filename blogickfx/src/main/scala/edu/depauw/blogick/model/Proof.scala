package edu.depauw.blogick.model

import cats.data.State

final case class ProofCheckException(msg: String) extends Exception(msg)

sealed trait Proof {
  val check: State[Environment, CheckedProof]
}

final case class ImplIntro(hypothesis: String,  conclusion: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    hypForm <- Environment.genVar
    _ <- Environment.extend(hypothesis, hypForm)
    cc <- conclusion.check
    formula = Implication(hypForm, cc.formula)
    binding <- Environment.retract
  } yield CkImplIntro(formula, binding, cc)
}

final case class ImplElim(impl: Proof, arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    ci <- impl.check
    ca <- arg.check
    _ = Implication(ca.formula, formula).unify(ci.formula)
  } yield CkImplElim(formula, ci, ca)
}

final case object TrueIntro extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    (env, CkTrueIntro)
  }
}

final case class ConjIntro(first: Proof, second: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    cf <- first.check
    cs <- second.check
    formula = Conjunction(cf.formula, cs.formula)
  } yield CkConjIntro(formula, cf, cs)
}

final case class ConjElimFirst(conj: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    dummy <- Environment.genVar
    cc <- conj.check
    _ = Conjunction(formula, dummy).unify(cc.formula)
  } yield CkConjElimFirst(formula, cc)
}

final case class ConjElimSecond(conj: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    dummy <- Environment.genVar
    cc <- conj.check
    _ = Conjunction(dummy, formula).unify(cc.formula)
  } yield CkConjElimSecond(formula, cc)
}

final case class DisjIntroLeft(arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    other <- Environment.genVar
    ca <- arg.check
    formula = Disjunction(ca.formula, other)
  } yield CkDisjIntroLeft(formula, ca)
}

final case class DisjIntroRight(arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    other <- Environment.genVar
    ca <- arg.check
    formula = Disjunction(other, ca.formula)
  } yield CkDisjIntroRight(formula, ca)
}

final case class DisjElim(disj: Proof, leftName: String, leftCase: Proof, rightName: String, rightCase: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    leftForm <- Environment.genVar
    rightForm <- Environment.genVar
    cd <- disj.check
    _ <- Environment.extend(leftName, leftForm)
    cl <- leftCase.check
    leftBind <- Environment.retract
    _ <- Environment.extend(rightName, rightForm)
    cr <- rightCase.check
    rightBind <- Environment.retract
    _ = Disjunction(leftBind.formula, rightBind.formula).unify(cd.formula)
    _ = formula.unify(cl.formula)
    _ = formula.unify(cr.formula)
  } yield CkDisjElim(formula, cd, leftBind, cl, rightBind, cr)
}

final case class FalseElim(falsum: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    cf <- falsum.check
    _ = False.unify(cf.formula)
  } yield CkFalseElim(formula, cf)
}

final case class NegIntro(hypothesis: String, contradiction: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    hypForm <- Environment.genVar
    formula = Negation(hypForm)
    _ <- Environment.extend(hypothesis, hypForm)
    cc <- contradiction.check
    binding <- Environment.retract
    _ = False.unify(cc.formula)
  } yield CkNegIntro(formula, binding, cc)
}

final case class NegElim(neg: Proof, arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    cn <- neg.check
    ca <- arg.check
    _ = Negation(ca.formula).unify(cn.formula)
  } yield CkNegElim(formula, cn, ca)
}

final case class Use(name: String) extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    env(name) match {
      case None => throw ProofCheckException(s"Binding not found: $name")
      case Some(formula) => (env, CkUse(formula, Binding(name, formula)))
    }
  }
}

final case object ToDo extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    val (env2, formula) = env.genVar()
    (env2, CkToDo(formula, env.bindings))
  }
}