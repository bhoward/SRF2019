package edu.depauw.blogick.model

import org.scalatest._

class ProofSpec extends FlatSpec with Matchers {
  "A simple proof" should "have the correct formula" in {
    val A = Variable(1)
    val proof = ImplIntro("x", Use("x"))
    val cp = proof.check.runA(Environment.Empty).value
    cp.formula should be(Implication(A, A))
  }

  "A proof with a hole" should "have the correct formula" in {
    val A = Variable(3)
    val B = Variable(2)
    val binding = Binding("x", Implication(A, A))
    val proof = ImplIntro("x", ImplElim(Use("x"), ToDo))
    val cp = proof.check.runA(Environment.Empty).value
    cp.formula should be(Implication(Implication(A, B), B))
  }

  "An invalid proof" should "throw ProofCheckException" in {
    val proof = Use("magic")
    a[ProofCheckException] should be thrownBy {
      proof.check.runA(Environment.Empty).value
    }
  }

  // a proof with a deliberate mix of => and ⇒:
  val proof = Proof.fromString(
    "x => y => <[left a ⇒ b ⇒ a y, right a ⇒ b => first b (a y)] x, left ([] y)>"
  )

  "A proof string" should "parse correctly" in {
    proof should be(
      ImplIntro(
        "x",
        ImplIntro(
          "y",
          ConjIntro(
            DisjElim(
              Use("x"),
              "a",
              ImplIntro("b", ImplElim(Use("a"), Use("y"))),
              "a",
              ImplIntro(
                "b",
                ImplElim(ConjElimFirst(Use("b")), ImplElim(Use("a"), Use("y")))
              )
            ),
            DisjIntroLeft(FalseElim(Use("y")))
          )
        )
      )
    )
  }

  it should "render correctly" in {
    proof.toString should be(
      "x ⇒ y ⇒ <[left a ⇒ b ⇒ a y, right a ⇒ b ⇒ first b (a y)] x, left ([] y)>"
    )
  }

  it should "have the correct formula" in {
    val cp = proof.check.runA(Environment.Empty).value
    cp.formula.toString should be("(⊥ → _9 ∨ ⊥ → _12) → ⊥ → ((_12 → _9 ∧ _11) → _9 ∧ (_14 ∨ _13))")
  }
}
