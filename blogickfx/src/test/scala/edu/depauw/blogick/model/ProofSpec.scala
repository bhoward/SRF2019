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
    proof.formula should be (Implication(A, A))
  }

  "A proof with a hole" should "have the correct formula" in {
    val A = Proposition("A")
    val binding = Binding("x", Implication(A, A))
    val proof = ImplElim(
      Use(binding),
      ToDo(Variable(0))
    )
    proof.formula should be (A)
  }
}