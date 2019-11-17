package edu.depauw.blogick.model

import org.scalatest._

class FormulaSpec extends FlatSpec with Matchers {
  val formula = Formula("A -> B")

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
    val formula2 = Formula("B -> A")
    a [UnificationException] should be thrownBy {
      formula.unify(formula2)
    }
  }
  
}