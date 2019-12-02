package edu.depauw.blogick.model

import org.scalatest._

class ProofSpec extends FlatSpec with Matchers {

  "A simple proof" should "have the correct formula" in {
    val A = Variable(1)
    val proof = ImplIntro("x", Use("x"))
    val cp = proof.check.runA(Environment.Empty).value
    cp.formula should be (Implication(A, A))
  }

  "A proof with a hole" should "have the correct formula" in {
    val A = Variable(3)
    val B = Variable(2)
    val binding = Binding("x", Implication(A, A))
    val proof = ImplIntro("x", ImplElim(Use("x"), ToDo))
    val cp = proof.check.runA(Environment.Empty).value
    cp.formula should be (Implication(Implication(A, B), B))
  }

  "An invalid proof" should "throw ProofCheckException" in {
    val proof = Use("magic")
    a [ProofCheckException] should be thrownBy {
      proof.check.runA(Environment.Empty).value
    }
  }
}