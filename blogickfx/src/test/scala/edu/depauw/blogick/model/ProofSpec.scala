package edu.depauw.blogick.model

import org.scalatest._

class ProofSpec extends FlatSpec with Matchers {

  "A simple proof" should "have the correct formula" in {
    val A = Proposition("A")
    val binding = Binding("x", A)
    val proof = ImplIntro(
      binding,
      Use(binding)
    )
    val cp = proof.check(Nil)
    cp.formula should be (Implication(A, A))
  }

  "A proof with a hole" should "have the correct formula" in {
    val A = Proposition("A")
    val binding = Binding("x", Implication(A, A))
    val proof = ImplElim(
      Use(binding),
      ToDo(Formula.genVar())
    )
    val cp = proof.check(binding :: Nil)
    cp.formula should be (A)
  }

  "An invalid proof" should "throw ProofCheckException" in {
    val A = Proposition("Magic")
    val proof = Use(Binding("magic", A))
    a [ProofCheckException] should be thrownBy {
      proof.check(Nil)
    }
  }
}