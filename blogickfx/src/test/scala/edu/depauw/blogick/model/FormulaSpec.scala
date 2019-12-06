package edu.depauw.blogick.model

import org.scalatest._

class FormulaSpec extends FlatSpec with Matchers {
  val formula = Formula.fromString("A -> B")
  val formula2 = Formula.fromString("A \\/ ~ A")

  "A simple formula" should "parse correctly" in {
    formula should be (Implication(Proposition("A"), Proposition("B")))
  }

  it should "render correctly" in {
    formula.toString should be ("A → B")
  }

  it should "unify correctly" in {
    val result = Variable(0)
    val formula2 = Implication(Proposition("A"), result)
    formula.unify(formula2)
    result.ref should be (Some(Proposition("B")))
  }

  it should "throw UnificationException if it doesn't match another formula" in {
    val formula2 = Formula.fromString("B -> A")
    a [UnificationException] should be thrownBy {
      formula.unify(formula2)
    }
  }
  
  "Another simple formula" should "parse correctly" in {
    formula2 should be (Disjunction(Proposition("A"), Implication(Proposition("A"), False)))
  }

  it should "render correctly" in {
    formula2.toString should be ("A ∨ ¬A")
  }

}