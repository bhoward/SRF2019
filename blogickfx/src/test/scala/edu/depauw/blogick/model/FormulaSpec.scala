package edu.depauw.blogick.model

import org.scalatest._

class FormulaSpec extends FlatSpec with Matchers {

  "A simple formula" should "parse correctly" in {
    val formula = Formula("A -> B")
    formula should be (Implication(Proposition("A"), Proposition("B")))
  }

  it should "throw UnificationException if it doesn't match another formula" in {
    val formula = Formula("A -> B")
    val formula2 = Formula("B -> A")
    a [UnificationException] should be thrownBy {
      formula.unify(formula2)
    }
  }
  
}