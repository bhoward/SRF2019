package edu.depauw.blogick.model

import cats.data.State

// TODO parse from text
// add top-level Theorem (maybe in CheckedProof?) -- give it a name and a formula (names for props);
//   allow use in other proofs (props as params);
//   keep track of completed theorems (no ToDo) and dependencies (no cycles);
//   allow update of ToDo with replacement proof

final case class ProofCheckException(msg: String) extends Exception(msg)

sealed trait Proof {
  val check: State[Environment, CheckedProof]

  def render(precedence: Int): String
  override def toString: String = render(0)

  def parenIf(level: Int, precedence: Int)(s: String): String =
  if (precedence >= level) {
    "(" + s + ")"
  } else {
    s
  }
}

final case class ImplIntro(hypothesis: String,  conclusion: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    hypForm <- Environment.genVar
    _ <- Environment.extend(hypothesis, hypForm)
    cc <- conclusion.check
    formula = Implication(hypForm, cc.formula)
    binding <- Environment.retract
  } yield CkImplIntro(formula, binding, cc)

  def render(precedence: Int): String = parenIf(1, precedence)(s"$hypothesis ⇒ ${conclusion.render(0)}")
}

final case class ImplElim(impl: Proof, arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    ci <- impl.check
    ca <- arg.check
    _ = Implication(ca.formula, formula).unify(ci.formula)
  } yield CkImplElim(formula, ci, ca)

  def render(precedence: Int): String = parenIf(2, precedence)(s"${impl.render(1)} ${arg.render(2)}")
}

final case object TrueIntro extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    (env, CkTrueIntro)
  }

  def render(precedence: Int): String = "<>"
}

final case class ConjIntro(first: Proof, second: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    cf <- first.check
    cs <- second.check
    formula = Conjunction(cf.formula, cs.formula)
  } yield CkConjIntro(formula, cf, cs)

  def render(precedence: Int): String = s"<${first.render(0)}, ${second.render(0)}>"
}

final case class ConjElimFirst(conj: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    dummy <- Environment.genVar
    cc <- conj.check
    _ = Conjunction(formula, dummy).unify(cc.formula)
  } yield CkConjElimFirst(formula, cc)

  def render(precedence: Int): String = parenIf(2, precedence)(s"first ${conj.render(2)}")
}

final case class ConjElimSecond(conj: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    dummy <- Environment.genVar
    cc <- conj.check
    _ = Conjunction(dummy, formula).unify(cc.formula)
  } yield CkConjElimSecond(formula, cc)

  def render(precedence: Int): String = parenIf(2, precedence)(s"second ${conj.render(2)}")
}

final case class DisjIntroLeft(arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    other <- Environment.genVar
    ca <- arg.check
    formula = Disjunction(ca.formula, other)
  } yield CkDisjIntroLeft(formula, ca)

  def render(precedence: Int): String = parenIf(2, precedence)(s"left ${arg.render(2)}")
}

final case class DisjIntroRight(arg: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    other <- Environment.genVar
    ca <- arg.check
    formula = Disjunction(other, ca.formula)
  } yield CkDisjIntroRight(formula, ca)

  def render(precedence: Int): String = parenIf(2, precedence)(s"right ${arg.render(2)}")
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

  def render(precedence: Int): String = parenIf(2, precedence)(s"[left $leftName ⇒ ${leftCase.render(0)}, right $rightName ⇒ ${rightCase.render(0)}] ${disj.render(2)}")
}

final case class FalseElim(falsum: Proof) extends Proof {
  val check: State[Environment, CheckedProof] = for {
    formula <- Environment.genVar
    cf <- falsum.check
    _ = False.unify(cf.formula)
  } yield CkFalseElim(formula, cf)

  def render(precedence: Int): String = parenIf(2, precedence)(s"[] ${falsum.render(2)}")
}

final case class Use(name: String) extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    env(name) match {
      case None => throw ProofCheckException(s"Binding not found: $name")
      case Some(formula) => (env, CkUse(formula, Binding(name, formula)))
    }
  }

  def render(precedence: Int): String = name
}

final case object ToDo extends Proof {
  val check: State[Environment, CheckedProof] = State { env =>
    val (env2, formula) = env.genVar()
    (env2, CkToDo(formula, env.bindings))
  }

  def render(precedence: Int): String = "?"
}

object Proof {
  import fastparse._
  import fastparse.SingleLineWhitespace._
  import fastparse.Parsed.Success
  import fastparse.Parsed.Failure
    
  private def idStart(c: Char): Boolean = c.isUnicodeIdentifierStart
  private def idPart(c: Char): Boolean = c.isUnicodeIdentifierPart || (c == '\'')

  private def id[_: P]: P[String] = P(
    (CharPred(idStart)~~CharsWhile(idPart).?).!
  )

  private def prim[_: P]: P[Proof] = P(
    id.map(Use(_))
  | "?".!.map(_ => ToDo)
  | "(" ~/ parser ~ ")"
  | "<" ~/ (">".!.map(_ => TrueIntro) | (parser ~ "," ~ parser ~ ">").map {
      case (first, second) => ConjIntro(first, second)
    })
  )

  private def appl[_: P]: P[Proof] = ??? // TODO

  private def parser[_: P]: P[Proof] = P(
    (id ~ ("=>" | "⇒") ~/ parser).map {
      case (hypothesis, conclusion) => ImplIntro(hypothesis, conclusion)
    }
  | appl
  ) // TODO use a repetition combinator
}